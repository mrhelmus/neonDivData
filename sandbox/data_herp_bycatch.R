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


STOPPED HERE 
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
STOPPED HERE 

#'
#' @source <https://data.neonscience.org>
#' @references Hoekman, David, Katherine E. LeVan, Cara Gibson, George E. Ball, Robert A. Browne, Robert L. Davidson, Terry L. Erwin, et al. “Design for Ground Beetle Abundance and Diversity Sampling within the National Ecological Observatory Network.” Ecosphere 8, no. 4 (2017): e01744.

"data_herp_bycatch"

#' @importFrom tibble tibble
NULL

# OLD CODE

library(tidyverse)

# Install and load devtools
# install.packages("devtools")
library(devtools)

# Install and load dev version of ecocomDP
# install_github("EDIorg/ecocomDP", ref = 'development')
library(ecocomDP)

# Install and load neonUtilities
# install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE)
library(neonUtilities)

include_zeros <- FALSE # Still need to figure out the zeros... is there a NULL taxonID?

#################################################################################
my_dpid <- 'DP1.10022.001' # beetle dpid to get herp bycatch
my_site_list <- c('BART','LAJA') # start with just one 

bycatch_raw <- neonUtilities::loadByProduct(
  dpID = my_dpid,
  site = my_site_list,
  check.size = FALSE#,
  #package = "expanded"
  ) # if there is a problem in the later code then use expanded

 # Kari's Mode function helper function to calculate mode of a column/vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### Wrangle Field Data ### 
# Edited From Kari's code

# 1. Get the fielddata table that contains metadata for all sampling events.
# 1. Clean eventID that provides the unique key for the events.
# 1. Select the important variables and toss the rest.
# 1. Calculate trappingDays as the length of time a pitfall trap was set

tidy_fielddata <- tibble::as_tibble(bycatch_raw$bet_fielddata) %>% # get fielddata
  dplyr::select(sampleID, # select needed variables
                domainID,
                siteID,
                namedLocation,
                trapID,
                setDate,
                collectDate,
                eventID,
                trappingDays) %>%
  dplyr::mutate(eventID =   # remove periods from eventID's (cleans an inconsistency)
                  stringr::str_remove_all(eventID, "[.]")) %>%
  dplyr::mutate(trappingDays = # add sampling effort in days (trappingdays)
                  lubridate::interval(lubridate::ymd(setDate),
                                      lubridate::ymd(collectDate)) %/%
                  lubridate::days(1))

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





