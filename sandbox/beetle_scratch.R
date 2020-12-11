# map_neon_data_to_ecocomDP.BEETLE <- function(
   neon.data.product.id = "DP1.10022.001"
#   ...){
#   # author: Kari Norman
#   
#   # download data
  beetles_raw <- neonUtilities::loadByProduct(dpID = neon.data.product.id,
                                              site = 'BART')
  
  # helper function to calculate mode of a column/vector
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  ### Clean Up Sample Data ###
  
  # start with the fielddata table, which describes all sampling events
  data_beetles <- tibble::as_tibble(beetles_raw$bet_fielddata) %>%
    dplyr::select(sampleID, domainID, siteID, namedLocation,
                  trapID, setDate, collectDate, eventID, trappingDays) %>%
    # eventID's are inconsistently separated by periods, so we remove them
    dplyr::mutate(eventID = stringr::str_remove_all(eventID, "[.]")) %>%
    #calculate a new trapdays column
    dplyr::mutate(trappingDays = lubridate::interval(lubridate::ymd(setDate),
                                                     lubridate::ymd(collectDate)) %/%
                    lubridate::days(1))
  
  #Find the traps that have multiple collectDates/bouts for the same setDate
  #need to calculate their trap days from the previous collectDate, not the setDate
  adjTrappingDays <- data_beetles %>%
    select(namedLocation, trapID, setDate, collectDate, trappingDays, eventID) %>%
    group_by_at(vars(-collectDate, -trappingDays, -eventID)) %>%
    filter(n_distinct(collectDate) > 1) %>%
    group_by(namedLocation, trapID, setDate) %>%
    mutate(diffTrappingDays = trappingDays - min(trappingDays)) %>%
    mutate(adjTrappingDays = case_when(
      diffTrappingDays == 0 ~ trappingDays,
      TRUE ~ diffTrappingDays)) %>%
    select(-c(trappingDays, diffTrappingDays))
  
  data_beetles <- data_beetles %>%
    #update with adjusted trapping days where needed
    left_join(adjTrappingDays) %>%
    mutate(trappingDays = case_when(
      !is.na(adjTrappingDays) ~ adjTrappingDays,
      TRUE ~ trappingDays
    )) %>%
    select(-adjTrappingDays, -setDate) %>%
    # for some eventID's (bouts) collection happened over two days,
    # change collectDate to the date that majority of traps were collected on
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup() %>%
    # there are also some sites for which all traps were set and collect on the same date, but have multiple eventID's
    # we want to consider that as all one bout so we create a new ID based on the site and collectDate
    tidyr::unite(boutID, siteID, collectDate, remove = FALSE) %>%
    dplyr::select(-eventID) %>%
    # join with bet_sorting, which describes the beetles in each sample
    dplyr::left_join(beetles_raw$bet_sorting %>%
                       # only want carabid samples, not bycatch
                       dplyr::filter(sampleType %in% c("carabid", "other carabid")) %>%
                       dplyr::select(sampleID, subsampleID, sampleType, taxonID,
                                     scientificName, taxonRank, identificationReferences,
                                     individualCount),
                     by = "sampleID")
  
  ### Clean up Taxonomy of Samples ###
  
  #Some samples were pinned and reidentified by more expert taxonomists, replace taxonomy with their ID's (in bet_parataxonomist) where available
  data_pin <- data_beetles %>%
    dplyr::left_join(beetles_raw$bet_parataxonomistID %>%
                       dplyr::select(subsampleID, individualID, taxonID, scientificName,
                                     taxonRank) %>%
                       dplyr::left_join(dplyr::distinct(dplyr::select(beetles_raw$bet_expertTaxonomistIDProcessed, taxonID, family))),
                     by = "subsampleID") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), "sort", "pin")) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  # some subsamples weren't fully ID'd by the pinners, so we have to recover the unpinned-individuals
  lost_indv <- data_pin %>%
    dplyr::filter(!is.na(individualID)) %>%
    dplyr::group_by(subsampleID, individualCount) %>%
    dplyr::summarise(n_ided = dplyr::n_distinct(individualID)) %>%
    dplyr::filter(n_ided < individualCount) %>%
    dplyr::mutate(unidentifiedCount = individualCount - n_ided) %>%
    dplyr::select(subsampleID, individualCount = unidentifiedCount) %>%
    dplyr::left_join(dplyr::select(data_beetles, -individualCount), by = "subsampleID") %>%
    dplyr::mutate(identificationSource = "sort")
  
  # add unpinned-individuals back to the pinned id's, adjust the individual counts so pinned individuals have a count of 1
  data_pin <- data_pin %>%
    dplyr::mutate(individualCount = ifelse(identificationSource == "sort", individualCount, 1)) %>%
    dplyr::bind_rows(lost_indv)
  
  
  #Join expert ID's to beetle dataframe
  data_expert <- dplyr::left_join(data_pin,
                                  dplyr::select(beetles_raw$bet_expertTaxonomistIDProcessed,
                                                individualID,taxonID,scientificName,
                                                taxonRank),
                                  by = 'individualID', na_matches = "never") %>%
    dplyr::distinct()
  
  #Update with expert taxonomy where available
  data_expert <- data_expert %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), identificationSource, "expert")) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  #Get raw counts table
  beetles_counts <- data_expert %>%
    dplyr::select(-c(subsampleID, sampleType, identificationSource, individualID)) %>%
    dplyr::group_by_at(dplyr::vars(-individualCount)) %>%
    dplyr::summarise(count = sum(individualCount)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  # get relevant location info from the data
  table_location_raw <- beetles_raw$bet_fielddata %>%
    dplyr::select(domainID, siteID, plotID, namedLocation, plotType, nlcdClass, decimalLatitude,
                  decimalLongitude, geodeticDatum, coordinateUncertainty,
                  elevation, elevationUncertainty) %>%
    dplyr::distinct()
  
  
  data_beetle <-  dplyr::left_join(beetles_counts, table_location_raw,
                                   by = c("domainID", "siteID", "namedLocation"))
  all(paste0(data_beetle$plotID, ".basePlot.bet") == data_beetle$namedLocation)
  # data_beetle = dplyr::select(data_beetle, -namedLocation)
  return(data_beetle)
}
