# 
# filePath <- "data/raw/floodplains/floodHazard.shp"
# data <- v1
# geometry <- geometryFiles[[1]]

processFlood <- function(geometry, name, data){
  # process the flood plain data 
  shp <- data |>
    dplyr::filter(ZONE_SUBTY =="FLOODWAY") |>
    sf::st_transform(crs = st_crs(geometry))
  
  # dataframe to hold information
  geom <- geometry |>
    dplyr::mutate(
      totalArea = sf::st_area(geometry),
      floodplainPercent = 0
    )|>
    as.data.frame()|>
    dplyr::select(GEOID, totalArea,floodplainPercent)
  
  # test for intersection with geometry -- index of flood plain features within the give geography
  t1 <- sf::st_intersects(geometry, shp, sparse = TRUE)
  
  for(i in 1:nrow(geom)){
    if(length(t1[[i]])==0){
      geom$floodplainPercent[i] <- 0
    }else{
      #subset floodplain data based on overlap
      # clip to area county boundaries
      f1 <- shp[t1[[i]], ]|>
        sf::st_intersection(geometry[i, ])
      # calculate total area
      t2 <- sum(sf::st_area(f1))
      geom$floodplainPercent[i] <- (t2 / geom$totalArea[i])*100
    }
  }
  output <- geom %>%
    dplyr::select("GEOID","floodplainPercent")
  
  return(output)
}




#' Generate noise measure
#'
#' @param filePath : location of noise raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getFlood <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  v1 <- sf::st_read(filePath)
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
