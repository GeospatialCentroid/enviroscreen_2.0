# 
# filePath <- "data/raw/lowBirthWeight/co_lowbirthweight_births_nosupp_1822.xlsx"
# data <- readxl::read_xlsx(filePath)
# geometry <- geometryFiles[[3]]
processLowBirthWeight <- function(geometry, name, data){
 
  
  # print statement to define process 
  print(paste0("Processing low birth rate data at the ", name, " geographic scale"))
  if(name == "county"){
    output <- data |> 
      dplyr::filter(geog == "County")|>
      dplyr::select("GEOID" = geoid, 
                    "lowBirthRate" = pct)
  }else{
    censustrack <- data |> 
      dplyr::filter(geog == "Census tract")|>
      dplyr::select("GEOID" = geoid, pct)
  }
  
  if(name == "censusTract"){
    output  <- censustrack |>
      dplyr::select(GEOID, "lowBirthRate" = pct)
  }
  
  if(name == "censusBlockGroup"){
    # shorten the cbg geoid and join to the data at census level. 
    output <- st_drop_geometry(geometry) |>
      dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) |>
      dplyr::left_join(censustrack,  by = c("GEOID2" = "GEOID")) |>
      dplyr::select(GEOID, "lowBirthRate" = pct)
  }
  
  return(output)
}




#' Generate LowBirthWeight measure
#'
#' @param filePath : location of LowBirthWeight raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getLowBirthWeight <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  d1 <- readxl::read_xlsx(filePath)
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/lowBirthWeight")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                        .y = names(geometryFiles),
                        .f = processLowBirthWeight,
                        data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/lowBirthWeight_", name , ".csv"))
  }
}
