# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processPOC <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- structureACS(vals) |>
    dplyr::group_by(GEOID)|>
    dplyr::mutate(
      percent_minority = ifelse(B03002_001 == 0,
                                NA,
                                (B03002_001 - B03002_003) /
                                  B03002_001))|>
    select("GEOID", "percent_minority")
  #export 
  return(output)
}


#' Generate poc measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getPOC <- function(geometryLayers){
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
  exportDir <- paste0(exportPathMain,"/poc")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processPOC,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/poc_", name , ".csv"))
  }
}
