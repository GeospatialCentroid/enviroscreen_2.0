# 
# data <- allData
# geometry <- geometryFiles[[2]]
# name <- names(geometryFiles)[[2]]

processDemoGraphics <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]]
  
  # pull the GEOID from the geometry object 
  geom <- geometry |>
    sf::st_drop_geometry()|>
    dplyr::select("GEOID")
  # read in and join datasets 
  for(i in 1:length(vals)){
    # read in data 
    d1 <- readr::read_csv(vals[i])
    # some dataset have a row number column... should change that in the export 
    if(ncol(d1) == 3){
      d1 <- d1[,2:3]
    }
    
    geom <- geom |>
      dplyr::left_join(y = d1, by = "GEOID")
  }
  
  
  # generate the percentile score  
  output <- geom |>
    dplyr::select(
      "GEOID",
      "Housing cost burden" = "HH_Burdened_Pct",
      "Percent disability" = "percent_disability",
      "Percent less than high school education" = "percent_lths",
      "Percent linguistic isolation" = "percent_lingiso",
      "Percent low income" =  "percent_lowincome",
      "Percent people of color" = "percent_minority"
    )|>
    calculateCumulativeDistance()
  # appply percentile calculations 
  output$demograpics <- output |>
    dplyr::select(contains("_pcntl"))|>
    apply(MARGIN = 1, FUN = gm_mean)
  # account for non populated areas 
  if(name !="county"){
    output <- removeZeroPopulation(data = output, name = name)
  }
  
  #export 
  return(output)
}


#' Generate housingBurden measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' 
getDemographics <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  data <- list.files("data/products/demographics",
                     pattern = ".csv",
                     full.names = TRUE,
                     recursive = TRUE)
  # organize for the function
  allData <- list(
    county = data[grepl(pattern = "county", x = data)],
    censusTract = data[grepl(pattern = "censusTract", x = data)],
    censusBlockGroup = data[grepl(pattern = "censusBlockGroup", x = data)]
  )
  
  
  # established the export 
  exportPathMain <- "data/products/componentScores"
  # create export dir
  exportDir <- exportPathMain
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processDemoGraphics,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/demographics_", name , ".csv"))
  }
}
