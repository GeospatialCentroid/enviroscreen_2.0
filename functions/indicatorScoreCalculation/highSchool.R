# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processHighSchool <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- structureACS(vals) |>
    dplyr::group_by(GEOID)|>
    dplyr::mutate(
      percent_lths = ifelse(
        B15002_001 == 0,
        NA,
        sum(
          B15002_003,
          B15002_004,
          B15002_005,
          B15002_006,
          B15002_007,
          B15002_008,
          B15002_009,
          B15002_010,
          B15002_020,
          B15002_021,
          B15002_022,
          B15002_023,
          B15002_024,
          B15002_025,
          B15002_026,
          B15002_027
        ) / B15002_001
      ))|>
    select("GEOID", "percent_lths")
  #export 
  return(output)
}


#' Generate highSchool measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getHighSchool <- function(geometryLayers){
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
  exportDir <- paste0(exportPathMain,"/highSchool")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processHighSchool,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/highSchool_", name , ".csv"))
  }
}
