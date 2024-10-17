# 
# data <- allData
# geometry <- geometryFiles[[2]]
# name <- names(geometryFiles)[[2]]

processSensitivePopulation <- function(geometry, name, data){
  # select the data set of interest 
  vals <- data[[grep(pattern = name, x = names(data))]]
  
  # pull the GEOID from the geometry object 
  geom <- geometry |>
    sf::st_drop_geometry()|>
    dplyr::select("GEOID")
  # read in and join datasets 
  for(i in 1:length(vals)){
    print(i)
    # read in data 
    d1 <- readr::read_csv(vals[i])
    print(names(d1))
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
      "Asthma" = "asthma",
      "Cancer" = "combinedCancer",
      "Diabetes" = "combinedDiabetes",
      "Cadiovascular" = "combinedHeart",
      "Life expectancy" = "lifeExpectancy",
      "Low birth weight" = "lowBirthRate",
      "Mental health" = "adj_rate_Prevalence",
      "Population over 64" = "age_over65",
      "Population under 5" = "age_under5"    
    )|>
    calculateCumulativeDistance()
  
  output$sensitivePopulation <- output |>
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
getSensitivePopulation <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  data <- list.files("data/products/sensitivePopulation",
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
                         .f = processSensitivePopulation,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/sensitivePopulation_", name , ".csv"))
  }
}
