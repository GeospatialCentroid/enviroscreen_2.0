###
# Primary processing script for the data processing code base 
# script is used to established the working environment and call other processing scripts 
# 202406017 
# carverd@colostate.edu
###


# source libraries ---
pacman::p_load(terra, dplyr, stringr, sf, targets, tidycensus,
               furrr, tigris, tmap, readr)
tmap_mode("view")
## alt ; install you census api key if you want or provide as parameter in functions 
# tidycensus::census_api_key(key = "your key",
#                            install = TRUE,
#                            overwrite = TRUE)

# source helper function ---
## this allows you to source files from a specific folder.
source("utilities/loadFunctions.R")

# load local utilities ----------------------------------------------------
loadFunctions("utilities")
## generate folder structure 
createFolderStructures()

# load local functions ----
loadFunctions("functions")


# gatheringDataSources ---- 
## geographic layers  
pullCensusGeographies(overwrite = FALSE)
### generate a named list of all geometry objects used in the workflow
geometries <- processGeometryLayers()



# prepping for analysis  --------------------------------------------------
## adjust block populations 
if(!file.exists("data/processed/geographies/blocksWithAdjustedPop.gpkg")){
  source("scripts/adjustedBlockPopulation.R")
}
## blockGroup buffering 
### quite a long run time. > 5 minutes 
if(!file.exists("data/processed/geographies/bgNeighbors.RDS")){
  source("scripts/blockGroupBuffering.R")
}

vals <- readRDS("data/processed/geographies/bgNeighbors.RDS")
# set up environment ----
plan(multisession, workers = 3)





# Indicator Score Calculation ----
## the processing functions themselves need to work exclusive of the targets frame
## work. Targets is being used here to ensure efficency in the running of the processing code
## work on what the best integration between the development stratigies is 

## Environmental Exposures ----
getNoise(filePath = "data/raw/noise/CONUS_L50dBA_sumDay_exi.tif",
         geometryLayers = geometries)
## environmental Effects ---- 


## climate vulnerability ----
getWildfire(filePath = "data/raw/wildfireRisk/Data/whp2023_GeoTIF/whp2023_cnt_conus.tif",
            geometryLayers = geometries)
## sensitive populations ----

## demographics score ----



# Component Score Calculations  -------------------------------------------




