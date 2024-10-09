#
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processEnviroScreen <- function(geometry, name, data){
  # select the data set of interest
  vals <- data[[grep(pattern = name, x = names(data))]]
  # sen pop
  v1 <- read_csv(vals[grepl(pattern = "HealthAndSocial", x = vals )])|>
    dplyr::select(
      "GEOID",
      "percent_disability","percent_disability_pcntl",
      "percent_lths","percent_lingiso_pcntl",
      "percent_lingiso","percent_lths_pcntl",
      "percent_lowincome","percent_lowincome_pcntl",
      "percent_minority","percent_minority_pcntl",
      "demograpics",
      "asthma","asthma_pcntl",
      "combinedCancer","combinedCancer_pcntl",
      "combinedDiabetes","combinedDiabetes_pcntl",
       "combinedHeart","combinedHeart_pcntl",
      "lifeExpectancy","lifeExpectancy_pcntl",
      "lowBirthRate",   "lowBirthRate_pcntl",
      "age_over65", "age_over65_pcntl",
      "age_under5", "age_under5_pcntl" ,
       "sensitivePopulation",
       "popCharacteristic"
    ) |>
    dplyr::mutate(
      scaledpopCharacteristic = popCharacteristic/max(popCharacteristic)*10
    )
  # enviromental effects
  v2 <- read_csv(vals[grepl(pattern = "PollutionAndClimate", x = vals )])
  
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
