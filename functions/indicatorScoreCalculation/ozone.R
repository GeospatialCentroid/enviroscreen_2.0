# 
# filePath <- "data/raw/epa_cmaq/2021_ozone_daily_8hour_maximum.txt.gz"
# data <- d1
# geometry <- geometryFiles[[1]]
processOzone <- function(geometry, name, data){
  
  # reassign the GEOID based on county FIPS 
  if(name == "county"){
    data$FIPS <- stringr::str_sub(data$FIPS, start = 1, end = 5)
  }
  # run the group by processinging and the GEOID 
  output <- data |>
    dplyr::mutate(Conc = as.numeric(`ozone_daily_8hour_maximum(ppb)`))|>
    dplyr::group_by(FIPS) %>%
    summarise(ozone_mean = mean(Conc)) %>%
    #rename as tract for calculation below
    dplyr::select(FIPS, ozone_mean)
  
  # join to census block 
  if(name == "censusBlockGroup"){
    output <- geometry |>
      sf::st_drop_geometry()|>
      dplyr::mutate(geoid2 = stringr::str_sub(GEOID, 1,11))|>
      dplyr::left_join(y = output, by = c("geoid2" = "FIPS"))|>
      dplyr::select(GEOID, ozone_mean)
  }
  #forcing the name and county and ct have FIPS still 
  names(output) <- c("GEOID","ozone_mean")
  return(output)
}




#' Generate ozone measure
#'
#' @param filePath : location of ozone raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getOzone <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # unzip data 
  extractedFile <- "data/raw/epa_cmaq/2021_ozone_daily_8hour_maximum.txt"
  if(!file.exists(extractedFile)){
    R.utils::gunzip(filePath,remove = FALSE)
  }
  ## read in the data and filter to colorado 
  d1 <- vroom::vroom(extractedFile) |>
    dplyr::filter(stringr::str_starts(FIPS, "08"))

  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/ozone")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                        .y = names(geometryFiles),
                        .f = processOzone,
                        data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/ozone_", name , ".csv"))
  }
}
