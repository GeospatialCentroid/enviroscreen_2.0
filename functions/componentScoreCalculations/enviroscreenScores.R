#
# data <- allData
# geometry <- geometryFiles[[2]]
# name <- names(geometryFiles)[[2]]

processEnviroScreen <- function(geometry, name, data){
  # select the data set of interest
  vals <- data[[grep(pattern = name, x = names(data))]]
  # sen pop
  v1 <- read_csv(vals[grepl(pattern = "HealthAndSocial", x = vals )])|>
    dplyr::select(
      "GEOID",
      "Housing cost burden", "Housing cost burden_pcntl",
      "Percent disability","Percent disability_pcntl",
      "Percent less than high school education","Percent less than high school education_pcntl",
      "Percent linguistic isolation","Percent linguistic isolation_pcntl",                  
      "Percent low income","Percent low income_pcntl",  
      "Percent people of color","Percent people of color_pcntl", 
      "demograpics",                                                                         
      "Asthma","Asthma_pcntl",
      "Cancer","Cancer_pcntl",
      "Diabetes","Diabetes_pcntl",
      "Cadiovascular","Cadiovascular_pcntl", 
      "Life expectancy","Life expectancy_pcntl",
      "Low birth weight","Low birth weight_pcntl",                       
      "Mental health","Mental health_pcntl",
      "Population over 64","Population over 64_pcntl",
      "Population under 5","Population under 5_pcntl",
      "sensitivePopulation",
      "popCharacteristic"
    )
  
  # exclude the NA values to calculat the full score 
  max <- max(v1$popCharacteristic, na.rm = TRUE)
  v1$scaledpopCharacteristic <- NA
  for(i in 1:nrow(v1)){
    rowVal <- v1$popCharacteristic[i]
    if(!is.na(rowVal)){
      v1$scaledpopCharacteristic[i] <- rowVal/max*10
    }
  }
  
  # enviromental effects
  v2 <- read_csv(vals[grepl(pattern = "PollutionAndClimate", x = vals )])|>
    dplyr::select(
      "GEOID",                                             
      "Air toxics emissions","Air toxics emissions_pcntl",                             
      "Diesel particulate matter","Diesel particulate matter_pcntl",                          
      "Drinking water regulations","Drinking water regulations_pcntl",                       
      "Lead exposure risk", "Lead exposure risk_pcntl",                               
      "Noise", "Noise_pcntl",                                           
      "Other air pollutants", "Other air pollutants_pcntl",                              
      "Ozone", "Ozone_pcntl",                                            
      "Fine particle pollution", "Fine particle pollution_pcntl",                           
      "Traffic proximity and volume", "Traffic proximity and volume_pcntl",                    
      "environmentalExposures",                           
      "Proximity to hazardous waste facilities", "Proximity to hazardous waste facilities_pcntl",          
      "Proximity to mining locations", "Proximity to mining locations_pcntl",             
      "Proximity to National Priorities List sites", "Proximity to National Priorities List sites_pcntl",     
      "Proximity to oil and gas", "Proximity to oil and gas_pcntl",                        
      "Proximity to Risk Management Plan sites", "Proximity to Risk Management Plan sites_pcntl",         
      "Impaired streams and rivers", "Impaired streams and rivers_pcntl",                      
      "Wastewater discharge", "Wastewater discharge_pcntl",                              
      "environmentalEffects",                             
      "Drought", "Drought_pcntl",                                          
      "Floodplains", "Floodplains_pcntl",                                      
      "Extreme heat days", "Extreme heat days_pcntl",                               
      "Wildfire risk", "Wildfire risk_pcntl",                                     
      "climateVulnerability",                            
      "pollutionClimateBurden" 
      )
  # exclude the NA values to calculat the full score 
  max <- max(v2$pollutionClimateBurden, na.rm = TRUE)
  v2$scaledpollClimate <- NA
  for(i in 1:nrow(v2)){
    rowVal <- v2$pollutionClimateBurden[i]
    if(!is.na(rowVal)){
      v2$scaledpollClimate[i] <- rowVal/max*10
    }
  }
  

  #bind datasets 
  output <- dplyr::left_join(v1, v2, by = "GEOID")|>
    dplyr::mutate(
      finalScore = scaledpollClimate * scaledpopCharacteristic,
      envExp_Pctl = cume_dist(environmentalExposures)*100,
      envEff_Pctl = cume_dist(environmentalEffects)*100,
      climate_Pctl = cume_dist(climateVulnerability)*100,
      senPop_Pctl = cume_dist(sensitivePopulation)*100,
      socEco_Pctl = cume_dist(demograpics)*100,
      pollClimBurden_Pctl = cume_dist(pollutionClimateBurden)*100,
      popCharacteristic_Pctl = cume_dist(popCharacteristic)*100,
      finalScore_Pctl = cume_dist(finalScore)*100
    )

  
  #export
  return(output)
}


#' Generate housingBurden measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#'
getEnviroScreen <- function(geometryLayers){
  # select geometry layers of interest
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data
  data <- list.files("data/products/groupComponentScores",
                      pattern = ".csv",
                      full.names = TRUE,
                      recursive = TRUE)

  # organize for the function
  allData <- list(
    county = data[grepl(pattern = "county", x = data)],
    censusTract = data[grepl(pattern = "censusTract", x = data)],
    censusBlockGroup = data[grepl(pattern = "censusBlockGroup", x = data)]
  )


  # established the export
  exportPathMain <- "data/products/enviroscreenScore"
  # create export dir
  exportDir <- exportPathMain
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processEnviroScreen,
                         data = allData)

  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/EnviroScreen_", name , ".csv"))
  }
}
