# 
## prep step on the data
# unzip("data/raw/wildfireRisk/RDS-2015-0047-4_Data.zip", exdir ="data/raw/wildfireRisk" )

# filePath <- "data/raw/wildfireRisk/Data/whp2023_GeoTIF/whp2023_cnt_conus.tif"
# data <- terra::rast(filePath)
# geometry <- geometryFiles[[1]]
processFire<- function(geometry, data){
  # convert to terra and project 
  geomVect <- terra::vect(geometry)|>
    terra::project(data)
  
  # extract Values
  extractedVals <- terra::extract(x = data, y = geomVect,fun = mean, na.rm = TRUE)
  
  # output
  output <- geomVect |>
    as.data.frame()|>
    dplyr::mutate(
      wildfire = extractedVals$Band_1
    )|>
    dplyr::select(GEOID, wildfire)
  return(output)  
}




#' Generate noise measure
#'
#' @param filePath : location of noise raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getWildfire <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  r1 <- terra::rast(filePath)
  # established the export 
  exportPathMain <- "data/products/climateVulnerability"
  # create export dir
  exportDir <- paste0(exportPathMain,"/wildfire")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map(.x = geometryFiles,
                         .f = processFire,
                         data = r1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/fire_", name , ".csv"))
  }
  
  #output the object
  return(results)
}
