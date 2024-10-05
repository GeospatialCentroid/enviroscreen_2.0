# 
# filePath <- "data/raw/epa_cmaq/2021_pm25_daily_average.txt.gz"
# data <- d1
# geometry <- geometryFiles[[1]]
name <- "censusBlockGroup"
processPM25 <- function(geometry, name, data){
  # reassign the GEOID based on county FIPS 
  if(name == "county"){
    data$FIPS <- stringr::str_sub(data$FIPS, start = 1, end = 5)
  }
  # run the group by processinging and the GEOID 
  output <- data |>
    dplyr::mutate(Conc = as.numeric(`pm25_daily_average(ug/m3)`))|>
    dplyr::group_by(FIPS) %>%
    summarise(pm25_mean = mean(Conc)) %>%
    #rename as tract for calculation below
    dplyr::select(FIPS, pm25_mean)

  # join to census block 
  if(name == "censusBlockGroup"){
    output <- geometry |>
      sf::st_drop_geometry()|>
      dplyr::mutate(geoid2 = stringr::str_sub(GEOID, 1,11))|>
      dplyr::left_join(y = output, by = c("geoid2" = "FIPS"))|>
      dplyr::select(GEOID, pm25_mean)
  }
  #forcing the name and county and ct have FIPS still 
  names(output) <- c("GEOID","pm25_mean")
  return(output)
}




#' Generate pm25 measure
#'
#' @param filePath : location of pm25 raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getPM25 <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # unzip data 
  extractedFile <- "data/raw/epa_cmaq/2021_pm25_daily_average.txt"
  if(!file.exists(extractedFile)){
    R.utils::gunzip(filePath,remove = FALSE)
  }
  ## read in the data and filter to colorado 
  d1 <- vroom::vroom(extractedFile) |>
    dplyr::filter(stringr::str_starts(FIPS, "08"))
  
  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/pm25")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processPM25,
                         data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/pm25_", name , ".csv"))
  }
}
