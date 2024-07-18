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


# function to run the processing 
## This should do all the spatial analysis tasks 
## the function above will handle the file management elements 
processingNoise <- function(geometry, layerName, noiseLayer){
  # print processing level 
  print(paste0("Processing ", layerName)
                    
  # read in spatial layer 
  # g2 <- sf::st_read(geometry) 
  
  # convert to terra object
  g3 <- terra::vect(geometry) |> 
    terra::project(noiseLayer)
  
  # grab values
  r2 <- terra::extract(noiseLayer, g3, mean, na.rm = TRUE)
  
  # attached GEOID to datasets
  geom <- dplyr::bind_cols(as.data.frame(g3), r2) |>
    dplyr::select(GEOID, noiseLevel = CONUS_L50dBA_sumDay_exi)
  # export 
  return(geom)
}

