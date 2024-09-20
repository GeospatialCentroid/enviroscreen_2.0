# 
# filePath <- "data/raw/noise/CONUS_L50dBA_sumDay_exi.tif"
# data <- terra::rast(filePath)  
# geometry <- geometryFiles[1]
processNoise <- function(geometry, data){
  # convert to terra and project 
  geomVect <- terra::vect(geometry)|>
    terra::project(data)

  # extract Values
  extractedVals <- terra::extract(x = data, y = geomVect,fun = mean, na.rm = TRUE)

  # output
  output <- geomVect |>
    as.data.frame()|>
    dplyr::mutate(
      noise = extractedVals$CONUS_L50dBA_sumDay_exi
    )|>
    dplyr::select(GEOID, noise)
  return(output)
}




#' Generate noise measure
#'
#' @param filePath : location of noise raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getNoise <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  r1 <- terra::rast(filePath)
  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/noise")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map(.x = geometryFiles,
                        # .y = names(geometryFiles),
                        .f = processNoise,
                        data = r1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/noise_", name , ".csv"))
  }
  
  #output the object
  return(results)
}
