#' Herpetofauna pitfall trap bycatch data collected by NEON
#'
#' This dataset was derived from [NEON data portal](https://data.neonscience.org) with data product ID 'DP1.10022.001'. Details about this data product can be found at <https://data.neonscience.org/data-products/DP1.10022.001>.
#'
#' The detailed design of NEON plant survey can be found in [Hoekman et al. 2017](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1744).

#' Here, we:
#'
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
my_site_list <- c('BART') # start with just one 

all_tabs <- neonUtilities::loadByProduct(
  dpID = my_dpid,
  site = my_site_list,
  check.size = FALSE,
  package = "expanded") #must use the expanded to get bycatch

# bet_fielddata has sampleID for all samples that were taken this can be used to 
# add zeros back in the herp data
# includes location and lat long data
bet_fielddata <- all_tabs$bet_fielddata

# get actual sampling data
herp_sorting<- all_tabs$bet_sorting %>% filter(sampleType == "vert bycatch herp")

index <- intersect(herp_sorting$sampleID, bet_fielddata$sampleID)

#inv_taxonomyProcessed <- all_tabs$bet_expertTaxonomistIDProcessed
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





