# 
# data <- allData
geometry <- geometryFiles[[3]]
name <- names(geometryFiles)[[3]]

processMentalHealth <- function(geometry, name, data){
  
  # Using the county level prevalance data for all grographies 
  d1 <- data$county |>
    dplyr::filter(Measure =="Frequent mental distress among adults",
                  Data_Value_Type == "Age-adjusted prevalence" )|>
    dplyr::mutate(GEOID = paste0("0",LocationID))|>
    dplyr::select(
      GEOID,
      adj_rate_Prevalence = Data_Value
    )
  
  
  # process based on geography 
  if(name == "county"){
    
    # join datasets and normalize the distributions 
    output <- d1 |>
      dplyr::select("GEOID","adj_rate_Prevalence")
  }
  # condition for census tract and census block group 
  if(name != "county"){
    # process the datasets 
    output <- data$tract |>
      dplyr::filter(Measure =="Frequent mental distress among adults",
                    Data_Value_Type == "Crude prevalence" )|>
      dplyr::mutate(GEOID = paste0("0",LocationID))|>
      dplyr::select(
        GEOID,
        adj_rate_Prevalence = Data_Value
      )
    
    # assign output based on geography name 
    if(name == "censusBlockGroup"){
      geometry$geoid2 <- str_sub(string = geometry$GEOID, start = 1, end = 11)
      # join to output and reformat
      output <- geometry |>
        sf::st_drop_geometry()|>
        dplyr::left_join(y =  output, by = c("geoid2"= "GEOID"))|>
        dplyr::select("GEOID","adj_rate_Prevalence")
    }
  }
  
  # output
  return(output)
}




#' Generate cancer measure
#'
#' @param filePath : location of cancer raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getMentalHealth <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  ## CDC data 
  cdcTracts <- read.csv("data/raw/CDC_places/PLACES_Tracts_24_CO.csv")
  cdcCounty <- read.csv("data/raw/CDC_places/PLACES_County_24_CO.csv")
  
  # gather dataset for the function  
  allData <- list(
    tract = cdcTracts,
    county = cdcCounty
  )
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/mentalHealth")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processMentalHealth,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/mentalHealth_", name , ".csv"))
  }
}
