#' Vertebrate Bycatch sampled from pitfall traps
#'
#' This dataset was derived from [NEON data portal](https://data.neonscience.org) with data product ID 'DP1.10022.001'. Details about this data product can be found at <https://data.neonscience.org/data-products/DP1.10022.001>.
#'
#' The detailed design of NEON plant survey can be found in [Hoekman et al. 2017](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1744).

#'
#' This dataset was derived from [NEON data portal](https://data.neonscience.org) with data product ID 'DP1.10022.001'. Details about this data product can be found at <https://data.neonscience.org/data-products/DP1.10022.001>.
#'
#' To process data we:
#' 1. Remove all non-carabid bycatch samples.
#' 2. Use the most expert taxonomy when available.
#' 3. Update abundances based on new taxonomy.
#' 4. Create a boutID that identifies all trap collection events at a site in the same bout (replacing eventID).
#' 5. Update collectDate to reference the most common collection day in a bout, maintaining one collectDate per bout.
#' 6. Create a new `trappingDays` column for the number of days a trap was set before being collected.
#' 7. Correct trap days to account for entries where the trap set date was not updated based on a previous collection.
#'
#' @note Details of locations (e.g. latitude/longitude coordinates can be found in [neon_locations]).
#'
#' @format A data frame (also a tibble) with the following columns:
#'
#' - `sampleID`: Identifier for sample.
#' - `siteID`: NEON site code.
#' - `plotID`: Plot identifier (NEON site code_XXX).
#' - `namedLocation`: Name of the measurement location in the NEON database.
#' - `trapID`: Identifier for trap.
#' - `setDate`: Date that trap was set.
#' - `collectDate`: Date of the collection event.
#' - `trappingDays`: Decimal days between trap setting and collecting events.
#' - `nativeStatusCode`: The process by which the taxon became established in the location.
#' 'A': Presumed absent, due to lack of data indicating a taxon's presence in a given location;
#' 'N': Native; 'I': Introduced; 'UNK': Status unknown.
#' - `identificationSource`: Source of identification.
#' - `taxonID`: Species code, based on one or more sources.
#' - `taxonRank`: The lowest level taxonomic rank that can be determined for the individual or specimen.
#' - `scientificName`: Scientific name, associated with the taxonID. This is the name of the lowest level taxonomic rank that can be determined.
#' - `count`: Number of individuals. `NA` represents no beetles catche in the trap.
#'
#' @author Kari Norman
#'
"data_beetle"


# STOPPED HERE 
#' 1. Removed 1 m^2 data with `targetTaxaPresent = N`
#' 2. Removed rows without plotID, subplotID, boutNumber, endDate, and/or taxonID
#' 3. Removed duplicate taxa between nested subplots (each taxon should be represented once for the bout/plotID/year). For example, if a taxon/date/bout/plot combo is present in 1 m^2 data, remove from 10 m^2 and above
#' 4. Stacked species occurrence from different scales into a long data frame. Therefore,
#'     + to get the species list at 1 m^2 scale, we need all the data with `sample_area_m2 == 1` (e.g. `dplyr::filter(plants, sample_area_m2 == 1)`); the unique sample unit id can then be generated with `paste(plants$plotID, plants$subplot_id, plants$subsubplot_id, sep = "_")`
#'     + to get the species list at 10 m^2 scale, we need all the data with `sample_area_m2 <= 10` (e.g. `dplyr::filter(plants, sample_area_m2 <= 10)`); the unique sample unit id can then be generated with `paste(plants$plotID, plants$subplot_id, plants$subsubplot_id, sep = "_")`
#'     + to get the species list at 100 m^2 scale, we need use the whole data set since the maximum value of `sample_area_m2` is 100 (i.e. a 10 m by 10 m subplot); the unique sample unit id can then be generated with `paste(plants$plotID, plants$subplot_id, sep = "_")`
#'     + to get the species list at 400 m^2 scale (i.e. one plot with four subplots), we need aggregate the data at `plotID` level (the sample unit is the plot now).
#'
#' @note  Details of locations (e.g. latitude/longitude coordinates can be found in [neon_locations]).
#' @format A data frame (also a tibble) with the following columns:
#'
#' - `namedLocation`: Name of the measurement location in the NEON database.
#' - `siteID`: NEON site code.
#' - `plotID`: Plot identifier (NEON site code_XXX).
#' - `subplotID`: This is the NEON provided subplot ID in the format of `subplot_id`, then `subsubplot_id` and 1 or 10 (m^2); if the sampling unit is 100 m^2, the values are 31, 32, 40, and 41.
#' - `boutNumber`: Number of sampling bout, mostly just 1.
#' - `year`: Observation year.
#' - `endDate`: The end date-time or interval during which an event occurred.
#' - `taxonID`: Species code, based on one or more sources.
#' - `scientificName`: Scientific name, associated with the taxonID. This is the name of the lowest level taxonomic rank that can be determined.
#' - `taxonRank`: The rank of `scientificName`; some of them are genus levels (we filtered out higher ranks already).
#' - `family`: The family of the species.
#' - `nativeStatusCode`: Whether the species is a native or non-native species. 'A': Presumed absent, due to lack of data indicating a taxon's presence in a given location; 'N': Native; 'N?': Probably Native; 'I': Introduced; 'I?': Probably Introduced; 'NI': Native and Introduced, some infrataxa are native and others are introduced; 'NI?': Probably Native and Introduced, some infrataxa are native and others are introduced; 'UNK': Status unknown.
#' - `percentCover`: Ocular estimate of cover of the index (e.g., species) as a percent within 1 m^2 subsubplots.
#' - `heightPlantOver300cm`: Indicator of whether individuals of the species in the sample are taller than 300 cm.
#' - `heightPlantSpecies`: Ocular estimate of the height (centimeter) of the plant species.
#' - `sample_area_m2`: The area of the sampling unit that the observed plant was located in.
#' - `subplot_id`: Subplot ID; one plot normally has four 100 m^2 subplots (31, 32, 40, 41).
#' - `subsubplot_id`: Subsubplot ID (1, 2, 3, 4) for sampling units at 1 m^2 or 10 m^2.
# STOPPED HERE 

#'
#' @source <https://data.neonscience.org>
#' @references Hoekman, David, Katherine E. LeVan, Cara Gibson, George E. Ball, Robert A. Browne, Robert L. Davidson, Terry L. Erwin, et al. “Design for Ground Beetle Abundance and Diversity Sampling within the National Ecological Observatory Network.” Ecosphere 8, no. 4 (2017): e01744.

"data_herp_bycatch"

#' @importFrom tibble tibble
NULL




#<>< HERE IS THE CODE


library(tidyverse)
library(neonUtilities)
library(devtools)
load_all()

#################################################################################
my_dpid <- 'DP1.10022.001' # beetle dpid to get herp bycatch
my_site_list <- c('BLAN','LAJA','SERC') # start with just one 

bycatch_raw <- beetles_raw <- beetles_raw_big

bycatch_raw <- neonUtilities::loadByProduct(
  dpID = my_dpid,
  site = my_site_list,
  check.size = FALSE#,
  #package = "expanded"
) # if there is a problem in the later code then use expanded


### Wrangle Field Data ### 
# Edited From Kari's code

# 1. Get the fielddata table that contains metadata for all sampling events.
# 1. Clean eventID that provides the unique key for the events.
# 1. Select the important variables and toss the rest.
# 1. Calculate trappingDays as the length of time a pitfall trap was set

tidy_fielddata <- tibble::as_tibble(bycatch_raw$bet_fielddata) %>% # get fielddata
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
  filter(n_distinct(collectDate) > 1) %>% # filter those with more than one
  mutate(totalSerialBouts = n_distinct(collectDate)) 

# check the data
# If the total setDates for any serial bout is >2 then the code may not work.
print(paste("The max serial bouts for any trap is", max(adjTrappingDays$totalSerialBouts)))

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
table(adjTrappingDays$adjTrappingDays)
filter(adjTrappingDays, adjTrappingDays == 0) # there are some that were set for one day
filter(adjTrappingDays, adjTrappingDays > 21) %>% print(n=200) # some that were set for a long time

# 1. Drop some columns from adjTrappingDays
adjTrappingDays <- adjTrappingDays %>% # drop some columns
  select(-c(trappingDays, diffTrappingDays, totalSerialBouts))

# everything is the same

# 1. Join trapping days to adjTrappingDays
tidy_fielddata <- tidy_fielddata %>%
  left_join(adjTrappingDays) 

# check the data
tidy_fielddata %>% 
  select(namedLocation, trappingDays, adjTrappingDays) %>% 
  filter(!is.na(adjTrappingDays)) %>% 
  print(n=200) # need to replace trapping days

#1. Adjust the trapping days
tidy_fielddata <- tidy_fielddata %>% 
  mutate(trappingDays = 
           case_when(!is.na(adjTrappingDays) ~ adjTrappingDays, # use adjTrappingDays
                     TRUE ~ trappingDays # otherwise just use trappingDays
  )) %>% select(-adjTrappingDays) # clean up the variables

# check the data
# for some eventID's (bouts) collection happened over two days
test <- select(tidy_fielddata, eventID, collectDate) %>% 
  unique() %>%
  group_by(eventID) %>% 
  summarize(n())
filter(tidy_fielddata, eventID == "ABBY201733") %>% print(n = 50)


# 1. Change collectDate to the date that majority of traps were collected on
Mode <- function(x) { # calculate mode of a column/vector
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tidy_fielddata <- tidy_fielddata %>%
  dplyr::group_by(eventID) %>%
  dplyr::mutate(collectDate = Mode(collectDate)) %>%
  dplyr::ungroup() 

# check the data
# there are also some sites for which all traps were set and collect on the same date, but have multiple eventID's
# we want to consider that as all one bout so we create a new ID based on the site and collectDate


# 1. Create a boutID that identifies all trap collection events at a site in the same bout (replacing eventID).

tidy_fielddata <- tidy_fielddata %>%
  tidyr::unite(boutID, siteID, collectDate, remove = FALSE) #%>%
  #dplyr::select(-eventID) # beetles dropped the eventID, here I keep it


# 1. join with bet_sorting that has the beetles and bycatch in each sample

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


# 1. perform full_join on fielddata and sorting to make sure to get all the sampleIDs and remove missing sampleIDs

tidy_fulldata <- tidy_fielddata %>%
  dplyr::full_join(tidy_sorting, by = "sampleID") %>%
  filter(!is.na(sampleID)) # remove all of the sampleIDs that are NAs

# check data - it seems like we have all of the sampleIDs and species from sorting joined ok

length(unique(tidy_fielddata$sampleID))
length(unique(tidy_sorting$sampleID))
setdiff(unique(tidy_fielddata$sampleID), unique(tidy_sorting$sampleID))
length(setdiff(unique(tidy_fielddata$sampleID), unique(tidy_sorting$sampleID)))
dim(tidy_fulldata)[1] - dim(tidy_sorting)[1]

test1 <- filter(tidy_fulldata, sampleID == "BLAN_002.N.20160526") # this sample is missing from sorting (as of writing this code)

test <- tidy_fulldata %>% 
  filter(is.na(sampleType)) %>%
  group_by(sampleID) %>%
  summarise(n())

# 1. Clean up the tidy_fulldata so that it is focused on vert bycatch herp

# sampleType provides the categories
unique(tidy_fulldata$sampleType) # note the NAs come from the fielddata without sorting data

tidy_fulldata_check <- tidy_fulldata %>% 
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

if(TRUE) # group all nonherps 
{
test <-  tidy_fulldata_check  %>%
  mutate(sampleType = 
           case_when(sampleType !="vert bycatch herp" ~ "not herp", # add a level for the missing sorting data
                     TRUE ~ sampleType))
test %>% dplyr::select(sampleID, sampleType, scientificName, individualCount) %>%
  group_by(sampleID, sampleType, scientificName) %>% arrange(sampleID) %>%
  mutate(individualCount_sum = sum(individualCount, na.rm = TRUE))
  
  
}

%>% unique()


# STOPPED HERE STOPPED HERE

# get actual sampling data
herp_sorting<- bycatch_raw$bet_sorting %>% filter(sampleType == "vert bycatch herp")

index <- intersect(herp_sorting$sampleID, bet_fielddata$sampleID)

herp_taxonomyProcessed <- herp_sorting %>% 
  dplyr::select(taxonID, scientificName, taxonRank) %>% 
  unique()

# REQUIRED TABLES -- format for 

# location
table_location <- bet_fielddata %>%
  select(namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
  distinct() %>%
  rename(
    location_id = namedLocation,
    latitude = decimalLatitude,
    longitude = decimalLongitude
  ) %>% filter(location_id %in% index)


# taxon
table_taxon <- herp_taxonomyProcessed %>%
  select(taxonID, taxonRank, scientificName) %>%
  distinct() %>%
  rename(taxon_id = taxonID,
         taxon_rank = taxonRank,
         taxon_name = scientificName)

#essential_cols = c(
#  "siteID", "plotID", "decimalLatitude", "decimalLongitude",
#  "taxonRank", "taxonID", "scientificName", "family", #"nativeStatusCode"
#)


# observation
table_observation <- herp_sorting %>% 
  dplyr::mutate(trappingDays = 
                  lubridate::interval(lubridate::ymd(setDate),
                                      lubridate::ymd(collectDate)) %/% 
                  lubridate::days(1)
  ) %>%
  select(sampleID,
         siteID,
         plotID,
         trapID,
         namedLocation,
         setDate,
         collectDate,
         trappingDays,
         nativeStatusCode,
         individualCount,
         taxonID) %>%
  rename(value = individualCount,
         observation_id = uid,
         event_id = sampleID,
         # package_id = NA,
         location_id = namedLocation,
         observation_datetime = collectDate,
         taxon_id = taxonID) %>%
  mutate(variable_name = 'abundance',
         unit = 'count per pitfall trap',
         package_id = NA) %>%
  #select(observation_id, event_id, package_id,
  #       location_id, observation_datetime,
  #       taxon_id, variable_name, value, unit) %>%
  select(siteID, plotID, namedLocation, trapID,
         observation_id, event_id, package_id,
         location_id, observation_datetime,
         taxon_id, variable_name, value, unit)

###################################
# write out in ecocomDP format
###
readr::write_csv(
  table_location,
  'table_location.csv')

readr::write_csv(
  table_taxon,
  'table_taxon.csv')

readr::write_csv(
  table_observation,
  'table_observation.csv')





