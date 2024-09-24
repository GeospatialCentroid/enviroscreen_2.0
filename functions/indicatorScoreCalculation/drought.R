
# structure  --------------------------------------------------------------
# 
# filePath <- "data/raw/drought/dm_export_20190101_20231231.csv"
# data <- read.csv(filePath)
# geometry <- geometries[[1]]

processDrought <- function(geometry, data){
  # combine three columns to show the percentage of counties within the three highest
  # drought categories at each time stamp 
  d1 <- data |>
    dplyr::mutate(FIPS = paste0("0",FIPS))|>
    rowwise() |>
    mutate(percentArea = sum(D2,D3,D4)) # drought intensity categories 
  # calculate average area in drought
  d2 <- d1  |>
    dplyr::select(FIPS,percentArea)  |>
    dplyr::group_by(FIPS)|>
    dplyr::summarise(averageAreaInDrought = mean(percentArea), sumAreaInDrought = sum(percentArea), totalWeeksConsidered = n())
  # calculate number of weeks with some drought
  d3 <- d1 |>
    dplyr::filter(percentArea != 0)|>
    dplyr::select(FIPS,percentArea)  |>
    dplyr::group_by(FIPS)|>
    dplyr::summarise(weeksWithDrought = n())
  # sum values in cat d2 d3 adn d4
  d5 <- dplyr::left_join(d2, d3, by ="FIPS")|>
    dplyr::mutate(
      percentTimeInDrought = (weeksWithDrought/totalWeeksConsidered)*100)
    
  # join to goemetry elements
  output <- sf::st_drop_geometry(geometry)|>
    dplyr::mutate("FIPS" = str_sub(GEOID, start = 1, end = 5))|>
    dplyr::left_join(d5, by ="FIPS")|>
    dplyr::select("GEOID",
                  "averageAreaInDrought",
                  "sumAreaInDrought",
                  "weeksWithDrought",
                  "percentTimeInDrought")
  
  return(output)
}




#' Generate drought measure
#'
#' @param filePath : location of drought raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getDrought <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  d1 <- read.csv(filePath)
  # established the export 
  exportPathMain <- "data/products/climateVulnerability"
  # create export dir
  exportDir <- paste0(exportPathMain,"/drought")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map(.x = geometryFiles,
                        .f = processDrought,
                        data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/drought_", name , ".csv"))
  }
}

