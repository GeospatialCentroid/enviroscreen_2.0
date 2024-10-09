#
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processPollutionAndClimate <- function(geometry, name, data){
  # select the data set of interest
  vals <- data[[grep(pattern = name, x = names(data))]]
  # environmentalExposures
  v1 <- read_csv(vals[grepl(pattern = "environmentalExposures", x = vals )])
  # environmentalEffects
  v2 <- read_csv(vals[grepl(pattern = "environmentalEffects", x = vals )])
  # climateVulnerability
  v3 <- read_csv(vals[grepl(pattern = "climateVulnerability", x = vals )])
  
  # bind 
  output <- dplyr::left_join(v1, v2, by= "GEOID")|>
    dplyr::left_join(y = v3, by = "GEOID")|>
    rowwise()|>
    dplyr::mutate(
      pollutionClimateBurden =  sum(environmentalExposures,
                                    (environmentalEffects * 0.5) ,
                                    (climateVulnerability *0.5),na.rm=TRUE)/2,
    )
  
  # not super happy with the column naming at the moment

  #export
  return(output)
}


#' Generate housingBurden measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#'
getPollutionAndClimate <- function(geometryLayers){
  # select geometry layers of interest
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data
  data1 <- list.files("data/products/componentScores",
                     pattern = ".csv",
                     full.names = TRUE,
                     recursive = TRUE)
  # subset for sensitive pop and demogrpahics
  data2 <- data1[grepl(pattern = "environmentalExposures",x = data1)]
  data3 <- data1[grepl(pattern = "environmentalEffects",x = data1)]
  data4 <- data1[grepl(pattern = "climateVulnerability",x = data1)]
  data <- c(data2,data3, data4)
  # organize for the function
  allData <- list(
    county = data[grepl(pattern = "county", x = data)],
    censusTract = data[grepl(pattern = "censusTract", x = data)],
    censusBlockGroup = data[grepl(pattern = "censusBlockGroup", x = data)]
  )


  # established the export
  exportPathMain <- "data/products/groupComponentScores"
  # create export dir
  exportDir <- exportPathMain
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processPollutionAndClimate,
                         data = allData)

  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/PollutionAndClimate_", name , ".csv"))
  }
}
