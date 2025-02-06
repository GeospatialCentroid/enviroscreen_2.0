###
# Primary processing script for the data processing code base 
# script is used to established the working environment and call other processing scripts 
# 202406017 
# carverd@colostate.edu
###

# source libraries ---
## test for and install pacman if needed
if("pacman" %in% rownames(installed.packages()) == FALSE){install.packages("pacman")}
# load the rest of the packages 
pacman::p_load(terra,
               dplyr,
               stringr,
               sf,
               tigris,
               readr,
               readxl,
               R.utils,
               vroom, 
               lubridate,
               sfdep)


## alt ; install you census api key if you want or provide as parameter in functions 
tidycensus::census_api_key(key = "your key here")

# source helper function ---
## this allows you to source files from a specific folder.
source("utilities/loadFunctions.R")

# load local utilities ----------------------------------------------------
loadFunctions("utilities")
## generate folder structure 
createFolderStructures()

# load local functions ----
loadFunctions("functions")


# define the overwrite value
## if you running for the first time set this to true 
## otherwise you'll same a lot of processing time by setting this to FALSE
## you can always alter the specific function parameter 
overwrite <- FALSE 

# gatheringDataSources ---- 
## geographic layers  
pullCensusGeographies(overwrite = overwrite)
### generate a named list of all geometry objects used in the workflow
geometries <- processGeometryLayers()

# prepping for analysis  --------------------------------------------------
## adjust block populations 
### produces the block centroid 2022 acs adjusted values 
if(!file.exists("data/processed/geographies/blocksWithAdjustedPop.gpkg")){
  source("scripts/adjustBlockPopulation.R")
}

## blockGroup buffering 
### quite a long run time. > 5 minutes 
if(!file.exists("data/processed/geographies/bgNeighbors.RDS")){
  source("scripts/blockGroupBuffering.R")
}
# read in the block group neighbors dataset, used in all buffer calculations 
blockGroupNeighbors <- readRDS("data/processed/geographies/bgNeighbors.RDS")


# Indicator Score Calculation ----

## EJScreen Data 
getEJscreen(geometryLayers = geometries, overwrite = TRUE)

# ACS data 
getACS(geometryLayers = geometries , overwrite =TRUE)

## Environmental Exposures ----
print("Environmental Exposures")
## air toxics  
getAir(filePath = "data/raw/haps/APENS_7_15_2024.xlsx",
       geometryLayers = geometries)

## diesel pm 
getDiesel(geometryLayers = geometries)

## drinking water 
getDrinkingWater(geometryLayers = geometries)

## lead 
getLead(geometryLayers = geometries)

## noise 
getNoise(filePath = "data/raw/noise/CONUS_L50dBA_sumDay_exi.tif",
         geometryLayers = geometries)

## other air pollutants 
getOtherAir(filePath = "data/raw/haps/APENS_7_15_2024.xlsx",
            geometryLayers = geometries)

## ozone 
getOzone(filePath = "data/raw/epa_cmaq/2021_ozone_daily_8hour_maximum.txt.gz",
         geometryLayers = geometries)
## pm2.5 
getPM25(filePath = "data/raw/epa_cmaq/2021_pm25_daily_average.txt.gz",
        geometryLayers = geometries)

## traffic 
getTraffic(geometryLayers = geometries)

## environmental Effects ---- 
print("Environmental Effects")
## impaired streams 
getStreams(filePath = "data/raw/surfaceWater/streams_303d_2024.shp",
           geometryLayers = geometries)

## hazardous waste 
getHazardousWaste(geometryLayers = geometries)

## mining
getMining(geometryLayers = geometries)

## NPL sites 
getNPLsites(geometryLayers = geometries)

## oil and gas 
getOilAndGas(geometryLayers = geometries)

## RMP sites 
getRMPsites(geometryLayers = geometries)

## wastewater discharge
getWasteWater(geometryLayers = geometries)


## climate vulnerability ----
## drought 
getDrought(filePath = "data/raw/drought/dm_export_20190101_20231231.csv",
           geometryLayers = geometries)
## heat days 
getHeat(folderPath = "data/raw/heatDays",
        geometryLayers = geometries)
## flood plain 
getFlood(filePath = "data/raw/floodplains/floodHazard.shp",  
         geometryLayers= geometries)
## wildfire
getWildfire(filePath = "data/raw/wildfireRisk/Data/whp2023_GeoTIF/whp2023_cnt_conus.tif",
            geometryLayers = geometries)



## sensitive populations ----
## asthma 
getAsthma(filePath = "data/raw/asthma/co_asthma_hospitalization_nosupp_1822.csv",
          geometryLayers = geometries)
## cancer 
getCancer(geometryLayers = geometries)
## heart disease 
getHeart(geometryLayers = geometries)
## diabetes
getDiabetes(geometryLayers = geometries)
## low birth weight 
getLowBirthWeight(filePath = "data/raw/lowBirthWeight/co_lowbirthweight_births_nosupp_1822.xlsx" ,
                  geometryLayers = geometries)

## life expectancy 
getLifeExpectency(filePath = "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv",
                  geometryLayers = geometries)

## mental health 
getMentalHealth(geometryLayers = geometries)
## pop under 5 
getUnder5(geometryLayers = geometries)

## pop over 65 
getOver65(geometryLayers = geometries)



## demographics score ----

## housing burden 
getHousingBurden(geometryLayers = geometries)

## percent disability 
### not measured at the census block group 
getDisability(geometryLayers = geometries)

## less then high school 
getHighSchool(geometryLayers = geometries)

## linguistic isolation 
getLinguisticIsolation(geometryLayers = geometries)

## low income 
getLowIncome(geometryLayers = geometries)

## people of color 
getPOC(geometryLayers = geometries)
  


# Component Score Calculations  -------------------------------------------
## environmental effects 
getEnvironmentalEffects(geometryLayers = geometries )

## environmental exposures 
getEnvironmentalExposures(geometryLayers = geometries )

## climate vulnerability 
getClimate(geometryLayers = geometries)

## sensitive population
getSensitivePopulation(geometryLayers = geometries)

## demographics 
getDemographics(geometryLayers = geometries)



#  Group Component Score Calculations  ------------------------------------
getHealthAndSocial(geometryLayers = geometries)


getPollutionAndClimate(geometryLayers = geometries)

# Enviroscreen Score Calculations  ----------------------------------------
getEnviroScreen(geometryLayers = geometries)

# process enviroscreen scores for export ----------------------------------
export <- TRUE
if(isTRUE(export)){
  source("scripts/exportEnviroScreenScores.R")
}

