# 
# filePath <- "data/raw/surfaceWater/streams_303d_2024.shp"
# data <- data
# geometry <- geometryFiles[[1]]
# name <- names(geometryFiles)[[1]]

processStreams <- function(geometry, name, data){
  
  streams <- data |>
    filter(substr(AUID, 1, 2) == "CO") # removing 4 "Lake" obs
  
  ### Organize the Data
  
  stream_uses <- streams |>
    mutate( # Uses
      AgUse = ifelse(Ag == "NA", 0, 1),
      AQLifeUse = ifelse(AQLife == "NA", 0, 1),
      RecUse = ifelse(Rec == "NA", 0, 1),
      WSUse = ifelse(WS == "NA", 0, 1),
      TotalUses = AgUse+AQLifeUse+RecUse+WSUse,
      # Impairment
      # ImpairedUse = ifelse(X303d_Uses_ > 0, 1, 0), # this category is current not in the datasets ask rani 
      # ImpairedUse_char = as.character(ImpairedUse),
      # PercentUsesImpaired = 100*X303d_Uses_/TotalUses,
      # Assessment status
      AgAssessed = ifelse(Ag == "X"| Ag == "NA", 0, 1),
      AQLifeAssessed = ifelse(AQLife == "X"| AQLife == "NA", 0, 1),
      RecAssessed = ifelse(Rec == "X"| Rec == "NA", 0, 1),
      WSAssessed = ifelse(WS == "X"| WS == "NA", 0, 1),
      TotalAssessed = AgAssessed+AQLifeAssessed+RecAssessed+WSAssessed,
      Assessed = ifelse(TotalAssessed > 0, 1, 0),
      Assessed_char = as.character(Assessed))
  
  
  
  
  
  
  
  
  
  
  
  # Using the county level prevalance data for all grographies 
  d1 <- data$county |>
    dplyr::filter(Measure =="Streams (non-skin) or melanoma among adults",
                  Data_Value_Type == "Age-adjusted prevalence" )|>
    dplyr::mutate(GEOID = paste0("0",LocationID))|>
    dplyr::select(
      GEOID,
      adj_rate_Prevalence = Data_Value
    )
  
  
  # process based on geography 
  if(name == "county"){
    
    # mortality data
    d2 <- data$mortality |>
      dplyr::filter(geog == "County")|>
      dplyr::mutate(
        GEOID = paste0("0", geoid),
        adj_rate = as.numeric(adj_rate)
      )|>
      dplyr::select(
        GEOID,
        "adj_rate_mortality" = adj_rate
      )
    
    # join datasets and normalize the distributions 
    output <- d1 |>
      dplyr::left_join(d2, by = "GEOID")|>
      dplyr::mutate(
        streamsPrevalence_pcntl = cume_dist(adj_rate_Prevalence)*100,
        streamsMortality_pcntl = cume_dist(adj_rate_mortality )*100
      ) |>
      dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","streamsPrevalence_pcntl", "streamsMortality_pcntl" )
  }
  # condition for census tract and census block group 
  if(name != "county"){
    # process the datasets 
    
    # mortality 
    d2 <- data$mortality |>
      dplyr::filter(geog == "Census tract")|>
      dplyr::mutate(
        GEOID = paste0("0", geoid)
      )|>
      dplyr::select(
        GEOID,
        "adj_rate_mortality" = adj_rate
      )
    # set county FIPS for join 
    d2$geoid2 <- stringr::str_sub(d2$GEOID, start = 1, end = 5)
    
    # join datasets 
    ## need to add a county GEOID to make the 
    output <- d2 |>
      dplyr::left_join(d1, by = c("geoid2" = "GEOID"))|>
      dplyr::mutate(
        streamsPrevalence_pcntl = cume_dist(adj_rate_Prevalence)*100,
        streamsMortality_pcntl = cume_dist(adj_rate_mortality )*100
      )|>
      dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","streamsPrevalence_pcntl", "streamsMortality_pcntl" )
    
    
    # assign output based on geography name 
    if(name == "censusBlockGroup"){
      geometry$geoid2 <- str_sub(string = geometry$GEOID, start = 1, end = 11)
      # join to output and reformat
      output <- geometry |>
        sf::st_drop_geometry()|>
        dplyr::left_join(y =  output, by = c("geoid2"= "GEOID"))|>
        dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","streamsPrevalence_pcntl", "streamsMortality_pcntl" )
    }
  }
  
  # output
  output <- output |>
    dplyr::rowwise()|>
    dplyr::mutate(combinedStreams = mean(c(streamsPrevalence_pcntl,streamsMortality_pcntl)))|>
    dplyr::select("GEOID", "combinedStreams")
  
  return(output)
}




#' Generate streams measure
#'
#' @param filePath : location of streams raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getStreams <- function(geometryLayers, filePath){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  data <- sf::st_read(filePath)
  
  # established the export 
  exportPathMain <- "data/products/environmentalEffects"
  # create export dir
  exportDir <- paste0(exportPathMain,"/streams")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processStreams,
                         data = data)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/streams_", name , ".csv"))
  }
}
