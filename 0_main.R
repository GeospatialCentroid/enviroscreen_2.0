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
source("utilities/loadFunctions.R")

# load local utilities ----------------------------------------------------
loadFunctions("utilities")
## generate folder structure 
createFolderStructures()

# load local functions ----
loadFunctions("functions")

# load local scripts ---- 
## not sure if want these written as functions or as scripts directly
## I think I'll lean toward functions for a bit more control of the variables... might need to rename
loadFunctions("scripts")


# gatheringDataSources ---- 
## geographic layers  
pullCensusGeographies(overwrite = FALSE)
## ACS data variables 
acsVars <- tidycensus::load_variables(year = 2022,dataset = "acs5")
write_csv(acsVars, file = "data/products/acsLabels.csv")

# set up environment ----
plan(multisession, workers = 3)



# Indicator Score Calculation ----
## the processing functions themselves need to work exclusive of the targets frame
## work. Targets is being used here to ensure efficency in the running of the processing code
## work on what the best integration between the development stratigies is 

## Environmental Exposures ----

## environmental Effects ---- 

## climate vulnerability ----

## sensitive populations ----

## demographics score ----



# Component Score Calculations  -------------------------------------------




