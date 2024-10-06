# 
# data <- allData
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processHousingBurden <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]] |> as.data.frame()
  
  # structure then generate and select measures of concern
  output <- structureACS(vals) |>
    dplyr::mutate(
      HHUnits = B25070_001+B25091_001, # renter total + owner total
      HH_Burdened = B25070_007+B25070_008+B25070_009+B25070_010+
        B25091_008+B25091_009+B25091_010+B25091_011+
        B25091_019+B25091_020+B25091_021+B25091_022, # >30% renters, mortgaged, nonmortgaged
      HH_Burdened_Pct = HH_Burdened/HHUnits)|>
    select("GEOID","HH_Burdened_Pct")
  #export 
  return(output)
}


#' Generate housingBurden measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getHousingBurden <- function(geometryLayers){
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
  exportDir <- paste0(exportPathMain,"/housingBurden")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processHousingBurden,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/housingBurden_", name , ".csv"))
  }
}
