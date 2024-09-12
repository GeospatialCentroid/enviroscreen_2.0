#' Generate noise measure
#'
#' @param filePath : location of noise raster data
#' @param geometry : spatial object representing the processing level
#' @param processingLevel : character description of the processing level
#' @param version : character description of the current version
#' @param overwrite : binary to overwrite existing content
#'
#' @return : dataframe with geoid and resulting data
#' 
#' 
#' 
#' 
getNoise <- function(filePath, geometryLayers){
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

  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
             .f = processingNoise,
             noiseLayer = r1)
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/noise_", name , ".csv"))
  }
  
  #output the object
  return(results)
}
