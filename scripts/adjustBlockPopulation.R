###
# stand alone script for
#
###


# Load required libraries
library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(stringr)

# Get block data for 2020
blocks <- tigris::blocks(state = "08", year = 2020) |>
  st_drop_geometry() |>
  mutate(
    cGEOID = str_sub(GEOID20, start = 1, end = 5),
    ctGEOID = str_sub(GEOID20, start = 1, end = 11),
    bgGEOID = str_sub(GEOID20, start = 1, end = 12)
  ) |>
  select("GEOID20",
         "INTPTLAT20",
         "INTPTLON20",
         "POP20",
         "cGEOID",
         "ctGEOID",
         "bgGEOID")

# Calculate total population for block groups in 2020
bg2020 <- blocks |>
  group_by(bgGEOID) |>
  summarize(totalPop2020 = sum(POP20, na.rm = TRUE)) |>
  select(bgGEOID, totalPop2020)

# Get block group data from 2022 ACS
bg2022 <- tidycensus::get_acs(
  geography = "cbg",
  variables = c("B01003_001"),# Total population
                state = "08",
                year = 2022) |>
    select(bgGEOID = GEOID,
           totalPop2022 = estimate)
  
  # Join block data with population data and calculate adjusted population
  blockJoin <- blocks |>
    left_join(y = bg2020, by = "bgGEOID") |>
    left_join(y = bg2022, by = "bgGEOID") |>
    mutate(
      # Adjust population based on changes between 2020 census and 2022 ACS
      acs2022PopAdj = case_when(POP20 == 0 ~ 0,
                                POP20 != 0 ~ round((POP20 / totalPop2020) * totalPop2022)),
      # Calculate percentage of block group population represented by each block
      percentOfCBGpopulation = round((acs2022PopAdj / totalPop2022) * 100, digits = 2)
    )

  
  
  
  # Create output directory if it doesn't exist
  outputDir <- "data/processed/geographies"
  dir.create(outputDir)
  
  # Export results as CSV and GPKG
  write.csv(blockJoin, file = paste0(outputDir, "/blocksWithAdjustedPop.csv"))
  
  # Convert to spatial object and write GPKG
  blockPopAbj <- blockJoin |>
    st_as_sf(coords = c("INTPTLON20", "INTPTLAT20")) |>
    st_set_crs(4326)
  # export the file 
  sf::st_write(blockPopAbj,
               paste0(outputDir, "/blocksWithAdjustedPop.gpkg"),
               delete_dsn = TRUE)
