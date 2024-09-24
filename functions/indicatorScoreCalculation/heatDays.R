
# structure  --------------------------------------------------------------
# because there are two files were using the folder path 
# filePath <- "data/raw/heatDays"
# data <- terra::rast(filePath)
# geometry <- geometryFiles[[1]]
processHeat <- function(geometry, name, data){
  
  # print statement to define process 
  print(paste0("Processing heat days data at the ", name, " geographic scale"))
  if(name == "county"){
    output <- data |> 
      dplyr::filter(is.na(CensusTract))|>
      dplyr::mutate(GEOID = paste0("0",CountyFIPS))|>
      dplyr::select(GEOID, Value) |>
      dplyr::group_by(GEOID)|>
      dplyr::summarise(aveHeatDays = mean(Value))
  }else{
    censustrack <- data |> 
      dplyr::filter(!is.na(CensusTract))|>
      dplyr::mutate(GEOID = paste0("0",CensusTract))|>
      dplyr::select(GEOID, Value)|>
      dplyr::group_by(GEOID)|>
      dplyr::summarise(aveHeatDays = mean(Value))
  }
  
  
  if(name == "censusTract"){
    output  <- censustrack |>
      dplyr::select(GEOID, "heatDays" = aveHeatDays)
  }
  # generate census block group measures 
  if(name == "censusBlockGroup"){
    # shorten the cbg geoid and join to the data at census level. 
    output <- st_drop_geometry(geometry) |>
      dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) |>
      dplyr::left_join(censustrack,  by = c("GEOID2" = "GEOID")) |>
      dplyr::select(GEOID, "heatDays" = aveHeatDays)
  }

  return(output)
}




#' Generate heat measure
#'
#' @param filePath : location of heat raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getHeat <- function(folderPath, geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  files <- list.files(
    path = folderPath, 
    pattern = ".csv",
    full.names = TRUE
  )
  d1 <- purrr::map(.x = files, .f = read.csv)|>
    dplyr::bind_rows()
  
  # established the export 
  exportPathMain <- "data/products/climateVulnerability"
  # create export dir
  exportDir <- paste0(exportPathMain,"/heat")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                        .y = names(geometryFiles),
                        .f = processHeat,
                        data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/heat_", name , ".csv"))
  }
}
