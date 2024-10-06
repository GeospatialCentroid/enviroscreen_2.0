# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processDisability <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- structureACS(vals) |>
    dplyr::mutate(
      percent_disability = sum(B18101_004, B18101_007,
                               B18101_010, B18101_013,
                               B18101_016, B18101_019,
                               B18101_023, B18101_026,
                               B18101_029, B18101_032,
                               B18101_035, B18101_038) / B18101_001)|>
    select("GEOID", "percent_disability")
  #export 
  return(output)
}


#' Generate disability measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getDisability <- function(geometryLayers){
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
  exportPathMain <- "data/products/demographics"
  # create export dir
  exportDir <- paste0(exportPathMain,"/disability")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processDisability,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/disability_", name , ".csv"))
  }
}
