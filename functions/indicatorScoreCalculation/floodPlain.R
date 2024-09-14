# 
# filePath <- "data/raw/noise/CONUS_L50dBA_sumDay_exi.tif"
# data <- terra::rast(filePath)  
# geometry <- geometryFiles[1]
processFlood <- function(geometry, name, data){

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
  v1 <- terra::vect(filePath)
  # established the export 
  exportPathMain <- "data/products/climateVulnerability"
  # create export dir
  exportDir <- paste0(exportPathMain,"/flood")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processFlood,
                         data = v1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/flood_", name , ".csv"))
  }
  
  #output the object
  return(results)
}
