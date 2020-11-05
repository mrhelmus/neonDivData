
  neon.data.product.id = "DP1.10022.001"
  # download data
  beetles_raw <- neonUtilities::loadByProduct(dpID = neon.data.product.id,
                                              ...)
  # saveRDS(beetles_raw, file = "~/Documents/allTabs_beetle.rds")
  # beetles_raw = readRDS("~/Documents/allTabs_beetle.rds")
  
  # helper function to calculate mode
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  # start data table with fielddata
  str_sub(beetles_raw$bet_fielddata$namedLocation, 10) %>% table() # just plot ID with basePlot.bet
  data_beetles <- tibble::as_tibble(beetles_raw$bet_fielddata) %>%
    # # there's an entry for every trap, whether or not they got samples, only want ones with samples
    # dplyr::filter(sampleCollected == "Y") %>%
    dplyr::select(sampleID, domainID, siteID,
                  # plotID,
                  namedLocation,
                  trapID, setDate, collectDate, eventID, trappingDays) %>%
    # eventID's are inconsistently separated by periods, so remove
    dplyr::mutate(eventID = stringr::str_remove_all(eventID, "[.]")) %>%
    dplyr::mutate(trappingDays = lubridate::interval(lubridate::ymd(setDate),
                                                     lubridate::ymd(collectDate)) %/%
                    lubridate::days(1))
  
  # join fielddata with bet_worting
  data_beetles <- data_beetles %>%
    # for some eventID's (bouts) collection happened over two days,
    # change collectDate to the date that majority of traps were collected on
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup() %>%
    # there are also some sites for which all traps were set and collect on the same date, but have multiple eventID's
    # we want to consider that as all one bout so we'll just create a new ID based on the site and collectDate
    tidyr::unite(boutID, siteID, collectDate, remove = FALSE) %>%
    dplyr::select(-eventID) %>%
    # and join to sample data
    dplyr::left_join(beetles_raw$bet_sorting %>%
                       # only want carabid samples, not bycatch
                       dplyr::filter(sampleType %in% c("carabid", "other carabid")) %>%
                       dplyr::select(# uid,
                         sampleID, subsampleID, sampleType,
                         taxonID, scientificName, taxonRank, identificationReferences,
                         individualCount, nativeStatusCode),
                     by = "sampleID") %>%
    # even though they were marked a sampled, some collection times don't acutally have any samples
    # dplyr::filter(!is.na(subsampleID)) %>% # do we want to remove samples without catch??
    dplyr::select(-boutID)
  
  
  # join data with bet_parataxonomistID
  # Join taxonomic data from pinning with the sorting data
  # Replace sorting taxon info with pinning taxon info (people that pin specimens
  # are more experienced with taxonomy), where available
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
  
  
  #Join expert data to existing pinning and sorting data
  #There are ~10 individualID's for which experts ID'd more than one species (not all experts agreed), we want to exclude those expert ID's as per Katie Levan's suggestion
  
  ex_expert_id <- beetles_raw$bet_parataxonomistID %>%
    dplyr::group_by(individualID) %>%
    dplyr::filter(dplyr::n_distinct(taxonID) > 1) %>%
    dplyr::pull(individualID)
  
  # Add expert taxonomy info, where available
  data_expert <- dplyr::left_join(data_pin,
                                  dplyr::select(beetles_raw$bet_parataxonomistID,
                                                individualID,taxonID,scientificName,
                                                taxonRank) %>%
                                    # exclude ID's that have unresolved expert taxonomy
                                    dplyr::filter(!individualID %in% ex_expert_id),
                                  by = 'individualID', na_matches = "never") %>%
    dplyr::distinct()
  
  # Replacement old taxon info with expert info, where available
  # NOTE - This is repetitive with the code snippet above, and if you want to do it
  # this way you can just combine the calls into one chunk. BUT, you may
  # want to do more than this, as it is *just* a replacement of IDs for individual
  # beetles that an expert identified. If the expert identified
  # a sample as COLSP6 instead of CARSP14, though, then all CARSP14 from that trap
  # on that date should probably be updated to COLSP6…
  data_expert <- data_expert %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(taxonID = ifelse(is.na(taxonID.y), taxonID.x, taxonID.y)) %>%
    dplyr::mutate(taxonRank = ifelse(is.na(taxonRank.y), taxonRank.x, taxonRank.y)) %>%
    dplyr::mutate(scientificName = ifelse(is.na(scientificName.y), scientificName.x, scientificName.y)) %>%
    dplyr::mutate(identificationSource = ifelse(is.na(scientificName.y), identificationSource, "expert")) %>%
    dplyr::select(-ends_with(".x"), -ends_with(".y"))
  
  #Get raw counts table
  beetles_counts <- data_expert %>%
    dplyr::select(-c(subsampleID, sampleType, individualID)) %>%
    dplyr::group_by_at(dplyr::vars(-individualCount)) %>%
    dplyr::summarise(count = sum(individualCount)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  # get relevant location info from the data
  table_location_raw <- beetles_raw$bet_fielddata %>%
    dplyr::select(domainID, siteID, plotID, namedLocation,
                  plotType, nlcdClass, decimalLatitude, decimalLongitude, geodeticDatum,
                  coordinateUncertainty, elevation, elevationUncertainty) %>%
    dplyr::distinct()
  
  
  data_beetle = dplyr::left_join(beetles_counts, table_location_raw,
                                 by = c("domainID", "siteID", "namedLocation"))
  all(paste0(data_beetle$plotID, ".basePlot.bet") == data_beetle$namedLocation)
  # data_beetle = dplyr::select(data_beetle, -namedLocation)
  return(data_beetle)
}


