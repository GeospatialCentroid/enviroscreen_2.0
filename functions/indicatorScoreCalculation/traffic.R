# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processTraffic <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- vals |> 
    select("GEOID", "traffic")
  #export 
  return(output)
}


#' Generate traffic measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getTraffic <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  data <- list.files("data/processed/ejscreen",pattern = ".csv",full.names = TRUE)
  # organize for the function
  allData <- list(
    county = read_csv(data[grepl(pattern = "county", x = data)]),
    censusTract = read_csv(data[grepl(pattern = "censusTract", x = data)]),
    censusBlockGroup = read_csv(data[grepl(pattern = "censusBlockGroup", x = data)])
  )
  
  
  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/traffic")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processTraffic,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/traffic_", name , ".csv"))
  }
}
