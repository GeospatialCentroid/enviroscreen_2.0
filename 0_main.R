###
# Primary processing script for the data processing code base 
# script is used to established the working environment and call other processing scripts 
# 202406017 
# carverd@colostate.edu
###


# source libraries ---
pacman::p_load(terra, dplyr, stringr, sf, tidycensus,
               sfdep, tigris, tmap, readr, readxl,R.utils, vroom,
               lubridate)
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
  source("scripts/adjustBlockPopulation.R")
}
## blockGroup buffering 
### quite a long run time. > 5 minutes 
if(!file.exists("data/processed/geographies/bgNeighbors.RDS")){
  source("scripts/blockGroupBuffering.R")
}

vals <- readRDS("data/processed/geographies/bgNeighbors.RDS")
# set up environment ----





# Indicator Score Calculation ----
## the processing functions themselves need to work exclusive of the targets frame
## work. Targets is being used here to ensure efficency in the running of the processing code
## work on what the best integration between the development stratigies is 


### both the ejscreen dataset and acs data have indicators in multiple component score measures 
### as a result these will be processed in a single script and write to their specific locations 
## EJScreen Data 
getEJscreen(geometryLayers = geometries, overwrite = FALSE)

# ACS data 
getACS(geometryLayers = geometries , overwrite =FALSE)

## Environmental Exposures ----
## air toxics  
getAir(filePath = "data/raw/haps/APENS_7_15_2024.xlsx",
       geometryLayers = geometries)

## diesel pm 
getDiesel(geometryLayers = geometries)

## drinking water 
getDrinkingWater(geometryLayers = geometries)

## lead 
### ACS data --- what is used to call this measures? 
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
## impaired streams 
getStreams(filePath = "data/raw/surfaceWater/streams_303d_2024.shp",
           geometryLayers = geometries)

## hazardous waste 
getHazardousWaste(geometryLayers = geometries)

## mining
getMining(geometryLayers = geometries)

## NPL sites 
getNPSsites(geometryLayers = geometries)

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


