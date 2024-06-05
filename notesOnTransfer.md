
# Vision

Colorado EnviroScreen Version 1.0 launched on June 29, 2022. Since that initial release the datasets and methods used to process the EnviroScreen score have been unchanged. The Environmental Justice group at Colorado Department of Public Health and Environment(CDPHE) has been monitoring the use and feedback of the tool and has determine a course of action to develop and release a new version of the tool that improves upon the initial version. 

Much like the development of EnviroScreen 1.0, the alterations expected within the 2.0 version will be informed, reviewed, and evaluated through a community focused lens. A collaborative team of professionals from CDPHE, Colorado School or Public Heatlh, and Colorado State University have put for the suggested changes to the methods based on a comprehensive assessment of lessons learned from version 1.0, public feedback, super user feedback, external review, and academic evaluations. The suggested changes will be out for public review. Evaluation of that public review will determine the direction of some of the changes in EnviroScreen 2.0. You can follow the development of these changes [here](https://cdphe.colorado.gov/colorado-enviroscreen-20-development-and-updates). 

This repository contains the code base for all data processing and metrics calculations within EnviroScreen 2.0. 

<span style="background-color: #FFFF00">Until the publication of EnviroScreen 2.0 in Fall of 2024 all material within this repository is considered a draft and is subject to change.</span>



## Planning of changes  

In a foundational scene the method to calculate the EnviroScreen Score is not changing dramatically between 1.0 and 2.0. The score is still centered on the concepts laid out in the 1.0 version. 

**Colorado EnviroScreen does:**

- Show which areas in Colorado are more likely to have higher environmental health injustices.
- Identify areas in Colorado where government agencies can prioritize resources and work to reduce pollution and other sources of environmental injustice.
- Provide information to empower communities to advocate to improve public health and the environment.
- Identify geographic areas that meet the definition of disproportionately impacted communities under Colorado law (House Bill 23-1233, C.R.S. § 24-4-109(2)(b)(II)).

**Colorado EnviroScreen does not:**

- Define a healthy or unhealthy environment.
- Establish causal associations between environmental risks and health.
- Define all areas that may be affected by environmental injustice or specific environmental risks.
- Provide information about an individual person’s health status or environment.
- Take all environmental exposures into account.
- Tell us about smaller areas within a census block group that may be more vulnerable to environmental exposures than other areas.
- Provide information about non-human health or ecosystem risks.


Some but not all expected changes are listed below.  

- create a docker based processing environment for increased reproducibility 
- transition to 2020 census tract geographies 
- update and potential chance of data sources used to represent a specific component score 
- the alternation of specific methods used to calculate the area of effect of point source pollution
- updated to `sf` and `terra` spatial libraries 



## Pulling from EnviroScreen 1.0 code base 

An effort is being made to utilize the existing resource of the EnviroScreen 1.0 data [procressing code base](https://github.com/GeospatialCentroid/COEnviroScreen_dataProcessing) when possible. 

All scripts and functions from the 1.0 methodology will be assessed and assigned to one of the follow categories. 

- Keep : pull directly from 1.0 with limited alterations (ex : update dataset)
- Adapt : pull from 1.0 (ex : update dataset and change some minor processing steps)
- Redo : the process that the script or function performs will still be present in 2.0 but the code will be rewritten completely.  (New data and methods require a revamp of the processing)
- Remove : the process that the script or function performs will not be required in 2.0 



# Main 
*0_main.R*: Adapt 

- Primary script for calling all data processing function. Exclude R Shiny processing step but utilize the overall structure.  

*updateJustice40.R* : Redo

- stand alone script to updated the Justice40 layer. Integrate into 0_main 


# R/ 


## Core changes to scripts and functions 

> Develop the foldering structure to better differentiate the purpose of the functions. Considered [Indicator Score Calculation, Component Score Calculations, Pulling Source Data]

> Alter the primary work flow to center are these steps. Rather then having data pulled within the component score calculations scripts 

> Integrate the data processing functions from [here](https://github.com/dcarver1/cwr_wildgrapes/tree/main/R2/dataProcessing) to improve the writing of data and handling of file paths. 

> When possible include multiple source data gather from the provider at specific geographies (county, census tract, census block groups) rather then aggregate up. (mean(census tracts in county) == county)

> All Census geographics with Zero population will likely be removed. Need to be specific about ensure these are exclude from all causes where data is reported at the census geography. 

> We may need to change how the percentile rank calculation is perform. It's expected that all percentile ranges should exist between zero and one. 

*acs.R* : redo  

- pulled data from the census, needs some reworking and updating based on the new data sets. Need to include a pull of the margin of error estimates for all ACS data. 

*getAsthma.R* : Redo 

- Processes Asthma datasets, we're trying to move away from scaling up values when possible. If we can get the asthma dataset at both the census tract and county then we 

*bufferObjects.R* : Redo 

- Determine the area of effect for point source datsets. Expecting to be altering. 

*climate.R* : Adapt 

- Script that condenses all the climate indicator scores into a single workflow. Probably just minor changes 

*drinkingWater.R* : Keep 

- Original method for evaluating the drinking water risk based on the number of reported issues for each water distributor in the state. Update the input data, improve some column names.  

*drought.R* : Adapt

- Calculates the percentage of time in drought at a specific census geography. Update new dataset and potentially alter the timeline. 


*ej_screen.R* : Redo

- Function for reading in and selecting specific indicators from the Federal EJ Screen dataset. We're planning on gathering all census based indicators from the ACS data directly rather then from existing aggregated sources. This will require remove some indications from the function into the ACS function. Updated data and slim down the number of indicators.  

*environmentalExposures.R* : Keep

- Script for generating all the environmental effect component scores.   


*EnvironmentalEffects.R* : Keep

- Script for generating all the environmental effect component scores.   
 

*finalComponentScore.R* : Keep

- Script for combining all the component scores to determine final and group component scores. 

*floodplain.R* : Redo 

- Runs an intersection between a flood plain layer and the specific geographies. Expecting an updated data source and this code is very slow. Update data and improve processing time. 

*getCoal.R* : remove 

- defined coal counties. Was removed from EnviroScreen 1.0 after release. 

*getDI.R* :

- All Disproportionately Impacted Community layers are being gerenated outside of the EnviroScreen data processing code base.

*getDI_2023.R*: Remove 

- All Disproportionately Impacted Community layers are being gerenated outside of the EnviroScreen data processing code base.

*getDI_AQCC.R* : Remove 

- All Disproportionately Impacted Community layers are being gerenated outside of the EnviroScreen data processing code base.

*getDI_MHC.R* : Remove 

- All Disproportionately Impacted Community layers are being gerenated outside of the EnviroScreen data processing code base.

*GetJustice40.R* : Remove 

- Reads in a dataset with the Justice 40 records and joins it to the census tract layers. As Justice 40 areas are not part of the Disproportionately Impacted Community layer, I do not think we will need to maintain the processing here. The DI layer is developed and maintained outside of the EnviroScreen platform. 


*getOilGas.R* : Remove

- defined oil and gas counties. Was removed from EnviroScreen 1.0 after release.

*getRural.R* :  Remove 

- defined rural counties. Was removed from EnviroScreen 1.0 after release. 

*haps.R* : Adapt 

- Gathers and summarizes exposure data to a set of pollutants. Based on the buffer methodology which is planned to be altered. Update data and work in new buffer method. Also this runs very slow, if possible it be nice to build some efficiency into the methods where we are buffering a lot of points.  

*heartDisease.R* : Redo 

- calculates prevalence of hearth disease at specific geography. Expecting to integrate both prevalence and hospitalization in 2.0 

*heatDays.R* : Redo 

- Calculates number of heat days based on data at the Census Tract level. Attempt to gather data at the county level as well rather then aggregate up to county. Update data and include multiple scales if possilbe. 

*houseBurden.R* : Keep

- Script for gathered census data and caculating a housing burden. Ensure that census codes did not change. Update data. 

*joinDataFrames.R* : Adapt 

- helper function to bind dataframes throughout the method. At the very least move to /utils

*lifeExpectancy.R* : Adapt

- Pulls census tract level data and assigns it to a geography. A lot of complex joining steps that can be improved for clarity. Update data and improve clarity of processing code. 

*lowBirthWeight.R* : Keep 

- Pulls census tract level data and assigns it to a geography. Update to new dataset

*mining.R* : Adapt 

- Gathers and summarizes mining related dataest from a few independent sources. Applies a buffer methods. Update data and work in the new buffer method. 

*noise.R* : Keep

- Process a raster based layer to specific geography. Update for new dataset. 

*otherHaps.R* : Adapt
 
- Gathers and summarizes exposure data to a set of pollutants. Based on the buffer methodology which is planned to be altered. Update data and work in new buffer method. 

*ozone.R* : Redo

-  Calculates the average yearly Ozone concentrations. Pontentially utilizing a different dataset or different time averaging stratigy. Plan on rewritting the function. 

*placesData.R* : Keep 

- Selects and organized datasets from the CDC Places data. Update for new datasets. 

*pm25.R* : Adapt

- Function for processing the pm2.5 data to specific geographies. 

*processData.R* : Adapt

- Script that calls the component score scripts. Remove the ej_screen and acsData processing functions. 

*proxyOilGas.R* : Adapt 

- Gathers a few independent data sources and applied a buffering methodology. Need to adapt source data and potentially the buffering method. 

*sensitivePopulations.R* : Adapt  

- Script for generating all the sensitive population indicator scores. Pull all data pulling function out of this script.   

*shinyData.R* : Remove

- Aggregate and organized data to export for the R shiny application. We are not planning on using R Shiny for version 2.0 so this will need to be replace with a more generic export process. 


*socioEconomic.R* :  Adapt 

- Script for generating all the social economic indicator scores. Pull all data pulling function out of this script. 

*surfaceWater.R* : Keep 

- Established method as part of 1.0 development for evaluating surface water quality. Update data source

*wildfire.R* : Keep

- calculates the wildfire risk from a raster layer. Update data source


# R/utils

*createFolderStructure.R* : Adapt 

- useful helper function for establishing the folder structure on an initial run. Need to adapt to match the updated indicators and reference layers 

*getGeometryLayers.R* : Keep 

- Pulled established geometry layers used thoughtout data processing code. Alter years and update some code structure/documentation 

*gm_meam.R* : Keep 

- used to calculate the geometric mean between indicator scores 

*loadFunctions.R* : Keep 

- helper function to sources all functions

*normalizeVector.R* : Keep 

- normalizes a vector of numberical values

*patternLayer.R* : Remove 

- Function used to stylize map elements. 

*setSpatialData.R* : Keep 

- Assign the specific geographic layer for set geography 



# Conventions 

- Script vs functions : not a perfect definition but a script is generally calling multiple functions. A function typically complete a specific tasks and only calls functions form the utils folder 

- camelCase : use this text structure for naming objects/files/etc 

- Functions : give each function it's own file. Give the file the same name as the function. 




