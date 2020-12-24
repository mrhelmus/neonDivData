#' Vertebrate Herpetofauna Bycatch sampled from pitfall traps
#'
#' This dataset was derived from [NEON data portal](https://data.neonscience.org) with data product ID 'DP1.10022.001'. Details about this data product can be found at <https://data.neonscience.org/data-products/DP1.10022.001>.
#'
#' To process data we:
#' 1. Cleaned trappingDays.
#'    - So that is is the number of days a trap was set before being collected.
#'    - Correct trap days to account for entries where the trap set date was not updated based on a previous collection.
#' 2. Create a boutID that identifies all trap collection events at a site in the same bout essentially replacing eventID.
#' 3. Update collectDate to reference the most common collection day in a bout, maintaining one collectDate per bout.
#' 4. sampleType provides the group that was caught in the pit fall trap. This was changed to have three levels
#'    - "vert bycatch herp" - these are the samples 
#'    - "no data collected" - these samples in fielddata not in sorting 
#'    - "not herp" - this is a aggregate of all the other types "other carabid", "invert bycatch", "carabid", "vert bycatch mam"
#'
#' @note  This script was derived from the script written by Kari Norman to process the pit fall traps of beetles.
#' Additional variables were added and missing samples were retained in herp_bycatch. By filtering on sampleType 
#' the user can derive a data set that include only samples with herps or samples with herps and not herps.
#' @format A data frame (tibble) with the following columns:
#' - `namedLocation`: Name of the measurement location in the NEON database.
#' - `domainID`: Unique identifier of the NEON domain where the site is located.
#' - `boutID`: Identifies all trap collection events at a site in the same bout.
#' - `siteID`: NEON site code.
#' - `plotID`: Plot identifier (NEON site code_XXX).
#' - `trapID`: Identifier for trap.
#' - `nlcdClass`: National Land Cover Database Vegetation Type Name.
#' - `decimalLatitude`: The geographic latitude (in decimal degrees, WGS84) of the geographic center of the reference area.
#' - `decimalLongitude`: The geographic longitude (in decimal degrees, WGS84) of the geographic center of the reference area. 
#' - `elevation`: Elevation (in meters) above sea level.
#' - `setDate`: Date that trap was set
#' - `collectDate`: Date of the collection event
#' - `eventID`: Cleaned up identifier for the set of information associated with the event, which includes information about the place and time of the event
#' - `trappingDays`: Cleaned up decimal days between trap setting and collecting events
#' - `sampleCollected`: Indicator of whether a sample was collected
#' - `sampleID`: Identifier for sample
#' - `sampleCondition`: Condition of a sample
#' - `samplingImpractical`: Samples and/or measurements were not collected due to the indicated circumstance
#' - `remarksFielddata`: Technician notes; free text comments accompanying the record from fielddata table
#' - `eventID_raw`: Identifier for the set of information associated with the event, which includes information about the place and time of the event
#' - `subsampleID`: Unique identifier associated with each subsample per sampleID
#' - `sampleType`: Type of sample with categories: vert bycatch herp, no data collected, not herp
#' - `taxonID`: Species code, based on one or more sources only provided for vert bycatch herp
#' - `scientificName`: Scientific name, associated with the taxonID. This is the name of the lowest level taxonomic rank that can be determined only provided for vert bycatch herp
#' - `taxonRank`: The lowest level taxonomic rank that can be determined for the individual or specimen only provided for vert bycatch herp
#' - `identificationQualifier`: A standard term to express the determiner's doubts about the Identification only provided for vert bycatch herp
#' - `morphospeciesID`: Identifier for morphospecies only provided for vert bycatch herp
#' - `identificationReferences`: A list of sources (concatenated and semicolon separated) used to derive the specific taxon concept; including field guide editions, books, or versions of NEON keys used only provided for vert bycatch herp
#' - `individualCount`: Number of individuals of the same type, summed across types for all sampleTypes but vert bycatch herp
#' - `nativeStatusCode`: 	The process by which the taxon became established in the location only provided for vert bycatch herp
#' - `remarksSorting`: Technician notes; free text comments accompanying the record from sorting table
#'
#' See `bet_variables` for more metadata on these variables
#'
#' @source <https://data.neonscience.org>
#' @references Hoekman, David, Katherine E. LeVan, Cara Gibson, George E. Ball, Robert A. Browne, Robert L. Davidson, Terry L. Erwin, et al. “Design for Ground Beetle Abundance and Diversity Sampling within the National Ecological Observatory Network.” Ecosphere 8, no. 4 (2017): e01744.
#' @author Matt Helmus and Kari Norman
"data_herp_bycatch"
NULL


#################################################################################
map_neon_data_to_ecocomDP.HERP <- function(neon.data.product.id = "DP1.10022.001",
                                           ...) {
  # authors: Matt Helmus (mrhelmus@temple.edu) repurposed Kari Norman's beetle code
  
  ### Setup ####
  
  #library(tidyverse)
  #library(neonUtilities)
  #library(devtools)
  # bycatch_raw <- beetles_raw <- beetles_raw_big # if you already have the full data product in memory
  #neon.data.product.id <- 'DP1.10022.001' # beetle dpid to get herp bycatch
  
  # Switches to run when editing the code
  chkdata <- FALSE # run the code that checks the data
  group_nonherps <- TRUE # run the code that aggregates the non-herps into sampleType:not herp
  herps_only <- FALSE # run the code that only gives you the samples with herps
  print_summary <- FALSE # print a summary table of the data
  
  #### Get the Data ####
  
  bycatch_raw <- neonUtilities::loadByProduct(dpID = neon.data.product.id,
                                              check.size = FALSE#,
                                              # if there is a problem with code then use expanded
                                              #package = "expanded"
  ) 
  
  ### Wrangle Field Data ### 
  
  # 1. Get the fielddata table that contains metadata for all sampling events.
  # 1. Clean up the trappingDays, which is a metric of catch per unit effort  
  # 1. Clean eventID that provides the unique key for the events.
  # 1. Select the important variables and toss the rest.
  # 1. Calculate trappingDays as the length of time a pitfall trap was set
  
  tidy_fielddata <- tibble::as_tibble(bycatch_raw$bet_fielddata) # get field data
  
  # check data - there are some NAs in the trappingDays (unit effort)
  if(chkdata) any(is.na(tidy_fielddata$trappingDays))
  
  tidy_fielddata <- tidy_fielddata %>%
    dplyr::select(namedLocation,  # select needed variables
                  domainID,
                  siteID,
                  plotID,
                  trapID,
                  nlcdClass,
                  decimalLatitude,
                  decimalLongitude,
                  elevation,
                  setDate,
                  collectDate,
                  eventID,
                  trappingDays,
                  sampleCollected,
                  sampleID,
                  sampleCondition,
                  samplingImpractical,
                  remarksFielddata = remarks) %>%
    dplyr::mutate(eventID_raw = eventID, 
                  eventID = stringr::str_remove_all(eventID, "[.]")) %>% # remove periods from eventID's (cleans an inconsistency)
    dplyr::mutate(trappingDays = # add sampling effort in days (trappingdays)
                    lubridate::interval(lubridate::ymd(setDate),
                                        lubridate::ymd(collectDate)) %/%
                    lubridate::days(1))
  
  # check data - no more NAs, but there are some edits that need to be made below
  if(chkdata) any(is.na(tidy_fielddata$trappingDays))
  
  # 1. Make object with cleaned up serial bouts
  # There are some traps that have multiple bouts for the same setDate. This is 
  # indicated by having the same setDate but different collectDates for a trap.
  # This indicates that the traps were reset to being open and sampling after a bout.
  # This serial sampling can also be seen by having unique eventIDs for each of these
  # serial bouts. To remedy this situation, trapping days should be calculated from
  # the previous collectDate, not the recorded setDate. To do this:
  
  adjTrappingDays <- tidy_fielddata %>%
    select(namedLocation, # select needed variables 
           trapID, 
           setDate, 
           collectDate, 
           trappingDays, 
           eventID
    ) %>%
    group_by(namedLocation, trapID, setDate) %>%
    filter(n_distinct(collectDate) > 1) %>% # filter those with more than one collectDate
    mutate(totalSerialBouts = n_distinct(collectDate)) 
  
  # check the data
  # If the total setDates for any serial bout is >2 then the code may not work.
  if(chkdata){
    print(paste("The max serial bouts for any trap is", max(adjTrappingDays$totalSerialBouts)))
  }
  
  #1. take the difference and use this as the new trapping days
  adjTrappingDays <- adjTrappingDays %>%
    mutate(diffTrappingDays = # as long as there are only 2 bouts this works
             trappingDays - min(trappingDays)) %>% 
    mutate(adjTrappingDays = # if the difference is 0 then trappingDays
             case_when(diffTrappingDays == 0 ~ trappingDays, 
                       TRUE ~ diffTrappingDays)) # otherwise use the difference
  
  # check the data
  # There are long bouts and 0 day bouts, 
  # but for now leave in data
  if(chkdata){
    table(adjTrappingDays$adjTrappingDays)
    filter(adjTrappingDays, adjTrappingDays == 0) # there are some that were set for one day
    filter(adjTrappingDays, adjTrappingDays > 21) %>% print(n=200) # some that were set for a long time
  }
  
  # 1. Drop some columns from adjTrappingDays
  adjTrappingDays <- adjTrappingDays %>% # drop some columns
    select(-c(trappingDays, diffTrappingDays, totalSerialBouts))
  
  # 1. Join trapping days to adjTrappingDays
  tidy_fielddata <- tidy_fielddata %>%
    left_join(adjTrappingDays) 
  
  # check the data
  if(chkdata){
    tidy_fielddata %>% 
      select(namedLocation, trappingDays, adjTrappingDays) %>% 
      filter(!is.na(adjTrappingDays)) %>% 
      print(n=200) # need to replace trapping days
  }
  
  #1. Adjust the trapping days
  tidy_fielddata <- tidy_fielddata %>% 
    mutate(trappingDays = 
             case_when(!is.na(adjTrappingDays) ~ adjTrappingDays, # use adjTrappingDays
                       TRUE ~ trappingDays # otherwise just use trappingDays
             )) %>% select(-adjTrappingDays) # clean up the variables
  
  # check the data
  # for some eventID's (bouts) collection happened over two days
  if(chkdata){
    select(tidy_fielddata, eventID, collectDate) %>% 
      unique() %>%
      group_by(eventID) %>% 
      summarize(n())
    
    filter(tidy_fielddata, eventID == "ABBY201733") %>% print(n = 50)
  }
  
  # 1. Change collectDate to the date that majority of traps were collected on
  Mode <- function(x) { # calculate mode of a column/vector
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  tidy_fielddata <- tidy_fielddata %>%
    dplyr::group_by(eventID) %>%
    dplyr::mutate(collectDate = Mode(collectDate)) %>%
    dplyr::ungroup() 
  
  
  # 1. Create a boutID that identifies all trap collection events at a site in the same bout (replacing eventID).
  # there are some sites for which all traps were set and collect on the same date, but have multiple eventID's
  # we want to consider that as all one bout so we create a new ID based on the site and collectDate
  
  tidy_fielddata <- tidy_fielddata %>%
    tidyr::unite(boutID, siteID, collectDate, remove = FALSE) #%>%
  #dplyr::select(-eventID) # beetles dropped the eventID, here I keep it
  
  
  ### Wrangle the Sorting data ###
  
  # 1. join with bet_sorting that has the bycatch in each sample
  
  tidy_sorting <- beetles_raw$bet_sorting %>% # select the variables that make the most sense to keep
    dplyr::select(sampleID, 
                  subsampleID, 
                  sampleType, 
                  taxonID,
                  scientificName, 
                  taxonRank, 
                  identificationQualifier,
                  morphospeciesID,
                  identificationReferences,
                  individualCount,
                  nativeStatusCode,
                  remarksSorting  = remarks)
  
  # check data -  there are some sampleIDs in sorting that are not in fielddata for an unknown reason
  if(chkdata){
    setdiff(unique(tidy_sorting$sampleID),unique(tidy_fielddata$sampleID))
    filter(tidy_sorting, sampleID == "KONA_007.E.20200902") # this sample is missing from fielddata (as of writing this code)
    
    # check data -  there are some sampleIDs in fielddata that are not in sorting for an unknown reason
    length(unique(tidy_fielddata$sampleID))
    length(unique(tidy_sorting$sampleID))
    dim(tidy_fulldata)[1] - dim(tidy_sorting)[1]
    
    # check data - there are a lot of sampleIDs that are NAs in the fielddata  these seem to be ones that could not be collected
    tidy_fielddata %>%
      filter(is.na(sampleID))
    
    tidy_sorting %>%
      filter(is.na(sampleID))
  }
  
  ### Produce the Full Data Set ###
  
  # 1. perform full_join on fielddata and sorting to make sure to get all the sampleIDs and remove missing sampleIDs
  
  tidy_fulldata <- tidy_fielddata %>%
    dplyr::full_join(tidy_sorting, by = "sampleID") %>%
    filter(!is.na(sampleID)) # remove all of the sampleIDs that are NAs
  
  # check data - it seems like we have all of the sampleIDs and species from sorting joined ok
  if(chkdata){ 
    length(unique(tidy_fielddata$sampleID))
    length(unique(tidy_sorting$sampleID))
    setdiff(unique(tidy_fielddata$sampleID), unique(tidy_sorting$sampleID))
    length(setdiff(unique(tidy_fielddata$sampleID), unique(tidy_sorting$sampleID)))
    dim(tidy_fulldata)[1] - dim(tidy_sorting)[1]
    
    filter(tidy_fulldata, sampleID == "BLAN_002.N.20160526") # this sample is missing from sorting (as of writing this code)
    
    tidy_fulldata %>% 
      filter(is.na(sampleType)) %>%
      group_by(sampleID) %>%
      summarise(n())
  }
  
  # 1. Clean up the tidy_fulldata so that it is focused on vert bycatch herp
  
  # sampleType provides the categories
  if(chkdata) unique(tidy_fulldata$sampleType) # note the NAs come from the fielddata without sorting data
  
  tidy_fulldata <- tidy_fulldata %>% 
    mutate(sampleType = 
             case_when(is.na(sampleType) ~ "no data collected", # add a level for the missing sorting data
                       TRUE ~ sampleType)) %>%
    mutate(sampleCondition = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ sampleCondition)) %>%
    mutate(samplingImpractical = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ samplingImpractical)) %>%
    mutate(remarksFielddata = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ remarksFielddata)) %>%
    mutate(subsampleID = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ subsampleID)) %>%
    mutate(taxonID = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ taxonID)) %>% 
    mutate(taxonRank = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ taxonRank)) %>% 
    mutate(scientificName = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ scientificName)) %>% 
    mutate(identificationQualifier = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ identificationQualifier)) %>% 
    mutate(morphospeciesID = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ morphospeciesID)) %>% 
    mutate(identificationReferences = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ identificationReferences)) %>% 
    mutate(nativeStatusCode = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ nativeStatusCode)) %>%
    mutate(remarksSorting = 
             case_when(sampleType!="vert bycatch herp" ~ NA_character_, 
                       TRUE ~ remarksSorting)) 
  
  # 1. Decide if you don't care about any of the other sampleTypes
  
  if(group_nonherps) # group all nonherps 
  {
    tidy_fulldata <-  tidy_fulldata  %>%
      mutate(sampleType =
               case_when(sampleType != "vert bycatch herp" ~ "not herp", # add a level for the missing sorting data
                         TRUE ~ sampleType)) %>%
      group_by(sampleID, sampleType, scientificName) %>% 
      mutate(individualCount_sum = sum(individualCount, na.rm = TRUE)) %>% # sum up all of the not herps
      mutate(individualCount_sum = na_if(individualCount_sum, 0)) %>% # put NA back in the fielddata with missing sorting data
      mutate(individualCount = individualCount_sum) %>% # replace with the new cout
      dplyr::select(-individualCount_sum) %>% # clean up
      ungroup() # clean up
  }
  
  # 1. Remove the redundant rows 
  tidy_fulldata <- tidy_fulldata %>% unique()
  
  if(chkdata){ # do all of the columns have data
    tidy_fulldata %>% select_if(~sum(!is.na(.)) > 0)
    tidy_fulldata %>% filter(!is.na(morphospeciesID))
  }
  
  #1. Do you only want samples with herps?
  if(herps_only) {
    tidy_fulldata <- tidy_fulldata %>% filter(sampleType == "vert bycatch herp")
  }
  
  # check data - output a summary table of the data  
  if(print_summary) {
    tidy_fulldata %>% 
      group_by(siteID) %>% 
      filter(sampleType == "vert bycatch herp") %>%
      summarise( 'herp species richness' = n_distinct(scientificName, na.rm = TRUE),
                 'herp individualCount total' = sum(individualCount), 
                 '# traps with herps' = n_distinct(sampleID)) %>% print(n = 100)
  }
  
  # these NA rows are from the sorting data that is not in the fielddata
  if(chkdata) tidy_fulldata %>% filter(is.na(siteID))
  
  return(tidy_fulldata)
}



