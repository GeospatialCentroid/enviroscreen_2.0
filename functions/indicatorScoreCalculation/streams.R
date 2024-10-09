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
      TotalUses = AgUse+AQLifeUse+RecUse+WSUse)|>
    mutate( # imparment 
      impairedUse = ifelse(Cat == "5", 1, 0), # if the Cat == 5 then it has 303d impaired sections 
      AgImpaired = ifelse(Ag == "N", 1, 0),
      AQLifeImpaired = ifelse(AQLife == "N", 1, 0),
      RecImpaired = ifelse(Rec == "N", 1, 0),
      WSImpaired = ifelse(WS == "N", 1, 0),
      TotalImpaired = AgImpaired+AQLifeImpaired+RecImpaired+WSImpaired,
      PercentUsesImpaired = 100*TotalImpaired/TotalUses,
    ) |>
    dplyr::mutate(
      # Assessment status
      AgAssessed = ifelse(Ag == "X"| Ag == "NA", 0, 1),
      AQLifeAssessed = ifelse(AQLife == "X"| AQLife == "NA", 0, 1),
      RecAssessed = ifelse(Rec == "X"| Rec == "NA", 0, 1),
      WSAssessed = ifelse(WS == "X"| WS == "NA", 0, 1),
      TotalAssessed = AgAssessed+AQLifeAssessed+RecAssessed+WSAssessed,
      Assessed = ifelse(TotalAssessed > 0, 1, 0),
      Assessed_char = as.character(Assessed))


  #### Overlay streams and geographic boundaries ----
  geometry <- geometry|>
    st_transform(crs = st_crs(stream_uses))|>
    select("GEOID")
  
  
  # dataframe for holding results 
  geom <- data.frame(matrix(nrow = nrow(geometry), ncol = 3))
  names(geom) <- c("GEOID", "AvgPercentImpaired", "PcntUnassessed")
  for(i in seq_along(geometry$GEOID)){
    print(i)
    # assign geoid 
    g1 <- geometry[i, ]
    geom$GEOID[i] <- g1$GEOID
  
    # crop 
    cropStreams <- sf::st_crop(stream_uses,g1)
    #intersect
    overlay <- st_intersection(cropStreams, g1) # very slow  
    # test for now streams in area 
    if(nrow(overlay)==0){
      geom$AvgPercentImpaired[i] <- NA
      geom$PcntUnassessed[i] <- NA
    }else{
      overlay$seglength <- st_length(overlay)
      
      # generate the measures 
      d1 <-  overlay|>
        sf::st_drop_geometry()|> # drop stream segment geometry for faster processing.
        dplyr::mutate(
          #convert segment length in meters to miles
          stream_mi = as.numeric(seglength)*0.000621,
          
          # Calculate the numerator for average percent impaired:
          # Stream segment length multiplied by the percent of uses impaired.
          # These will be added together for the entire county in the
          # "summarise" step below.
          numerator_impaired = stream_mi*(PercentUsesImpaired/100),
          
          # Calculate the numerator for percent unassessed
          # Stream segment length for completely unassessed streams.
          # These will be added together for the entire county in the
          # "summarise" step below.
          numerator_completelyunassessed = ifelse(Assessed == 0, stream_mi, 0))|>
          dplyr::summarise(TotalStreamLengthMi = sum(stream_mi, na.rm = TRUE),
                         numerator_impaired = sum(numerator_impaired, na.rm = TRUE),
                         numerator_completelyunassessed = sum(numerator_completelyunassessed, na.rm = TRUE))|>
        ## because we are working on single counties we can not apply percent rank function until alfter all geometries
        ## have been resolved.
          mutate(AvgPercentImpaired = numerator_impaired/TotalStreamLengthMi,
                 PcntUnassessed = 100*numerator_completelyunassessed/TotalStreamLengthMi
        )
      
      geom$AvgPercentImpaired[i] <- d1$AvgPercentImpaired
      geom$PcntUnassessed[i] <- d1$PcntUnassessed
    }
  }
    # format for export 
    output <- geom |>
      dplyr::mutate(
        ImpairedPctl = percent_rank(AvgPercentImpaired)*100,
        UnassessedPctl = percent_rank(PcntUnassessed)*100,
        CombinedMetric = ImpairedPctl + UnassessedPctl/2) |>
      dplyr::select(
        "GEOID", "surfaceWater"= "CombinedMetric"
      )
  
  return(output)
}




#' Generate streams measure
#'
#' @param filePath : location of streams raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getStreams <- function(filePath,geometryLayers){
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
