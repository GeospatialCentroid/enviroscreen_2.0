# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processOver65 <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- structureACS(vals) |>
    dplyr::group_by(GEOID)|>
    dplyr::mutate(
      age_over65 = sum( B01001_020, B01001_021, B01001_022, B01001_023, B01001_024, B01001_025,
                        B01001_044, B01001_045, B01001_046, B01001_047, B01001_048, B01001_049))|>
    select("GEOID", "age_over65")
  #export 
  return(output)
}


#' Generate over65 measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getOver65 <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  data <- list.files("data/processed/acs",pattern = ".csv",full.names = TRUE)
  # organize for the function
  allData <- list(
    county = read_csv(data[grepl(pattern = "county", x = data)]),
    censusTract = read_csv(data[grepl(pattern = "censusTract", x = data)]),
    censusBlockGroup = read_csv(data[grepl(pattern = "censusBlockGroup", x = data)])
  )
  
  
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/over65")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processOver65,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/over65_", name , ".csv"))
  }
}
