# 
# filePath <- "data/raw/haps/APENS_7_15_2024.xlsx"
# data <- d1
# geometry <- geometryFiles[[1]]

processAir <- function(data){
  # read in reference layers 
  blocks <- sf::st_read("data/processed/geographies/blocksWithAdjustedPop.gpkg")
  # block group relations 
  blockGroupNeighbors <- readRDS("data/processed/geographies/bgNeighbors.RDS")
  # pull census block groups 
  cbg <- sf::st_read("data/processed/geographies/censusBlockGroup.gpkg")
  
  # drop site with no lat lon
  d1 <- data |>
    dplyr::filter(SITE_X_COORDINATE != 0)
  ### Generate 5 year mean values for all pollutants
  d2 <- d1 |>
    dplyr::select(APCD_SITE_ID,
                  SITE_100414_ESTIM,
                  SITE_106467_ESTIM,
                  SITE_106990_ESTIM,
                  SITE_18540299_ESTIM,
                  SITE_50000_ESTIM,
                  SITE_56235_ESTIM,
                  SITE_71432_ESTIM,
                  SITE_75070_ESTIM,
                  SITE_75218_ESTIM,
                  SITE_7782505_ESTIM,
                  SITE_822060_ESTIM,
                  SITE_91203_ESTIM,
                  SITE_ASC_ESTIM,
                  SITE_CE_ESTIM
    )|>
    dplyr::group_by(APCD_SITE_ID)%>%
    dplyr::summarise_all(mean ,na.rm = TRUE)
  
  ### normalize data based on volume of emission
  d2[,2:15] <- apply(d2[,2:15], MARGIN = 2, FUN = normalizeVector)
  ### calculate total
  d2$total <- rowSums(d2[,c(-1)], na.rm = TRUE)

  
  # create a spatial object 
  sp1 <- d1 |>
    dplyr::select("APCD_SITE_ID",   # rename for input into buffer process
                            "SITE_X_COORDINATE",
                            "SITE_Y_COORDINATE")|>
    st_as_sf(coords =c("SITE_X_COORDINATE","SITE_Y_COORDINATE"),crs=4269 )|>
    dplyr::left_join(y = d2, by = "APCD_SITE_ID" )|> 
    dplyr::filter(total != 0)|>
    dplyr::select("APCD_SITE_ID", "total") |>
    dplyr::distinct()|>
    sf::st_transform(crs = crs(cbg))
  

  # returns a postition index on the interestion of the cbg per each mining location
  t1 <- sf::st_intersects(x = sp1,
                          y = cbg,
                          sparse = TRUE
  )
  # when I unlist() the t1 object I'm looking a row... not sure why so using a for loop to assign data
  ## item 84, has a lat value outside of the state so it can not reference cbg 
  sp1$cbg_geoid <- NA
  
  for(i in 1:length(t1)){
    index <- cbg$GEOID[t1[[i]]]
    if(identical(index, character(0))){
      sp1$cbg_geoid[i] <- NA
    }else{
      sp1$cbg_geoid[i] <- index
    }
  }
  
  # remove any na values 
  sp1_clean <- sp1 |> dplyr::filter(!is.na(cbg_geoid))
  
  # define the site score value (different for each indicator. )
  sp1_clean$siteScore <- sp1_clean$total
  
  
  
  
  
  
  # define the index for the map function
  index <- 1:nrow(sp1_clean)
  # call the calculate score function 
  exportFile <-  "data/products/environmentalExposures/airQuality/detailsOnDistanceScoring.csv"
  
  ## conditional to avoid timely geoprocessing step 
  if(!file.exists(exportFile)){
    for(i in index){
      val <- calculateDistanceScore(index = i,
                                    sites = sp1_clean, 
                                    blockGroupNeighbors= blockGroupNeighbors,
                                    blocks = blocks )
      if(i == 1){
        scores <- val
      }else{
        scores <- scores |> bind_rows(val)
      }
    }
    
    # export here because this is a big geoprocessing step 
    write.csv(scores, file = exportFile)
  }else{
    scores <- readr::read_csv(exportFile)
  }
  
  
  formatedScores <- scores |> 
    # summarize to agggregate measures to the blockGEOID 
    dplyr::group_by(GEOID20)|>
    dplyr::summarise(aggregatedNoPopScore = sum(nonPopScore),
                     aggregatedPercentPopScore = sum(percentPopScore),
                     numberOfSource = n())
  write.csv(formatedScores, file = "data/products/environmentalExposures/airQuality/aggratedScoreValues.csv")
  
  # group these by census block group, census tract, county 
  allScores <- formatedScores |> 
    dplyr::mutate(
      cGEOID = stringr::str_sub(GEOID20, start = 1, end = 5),
      ctGEOID = stringr::str_sub(GEOID20, start = 1, end = 11),
      bgGEOID = stringr::str_sub(GEOID20, start = 1, end = 12)
    )
  write.csv(allScores, file = "data/products/environmentalExposures/airQuality/airQuality_census.csv")
  # 
  # generate aggregates score measures 
  ## county
  countyScores <- allScores |> 
    dplyr::group_by(cGEOID)|>
    dplyr::summarise(PercentPopScore = sum(aggregatedPercentPopScore))|>
    dplyr::select(
      "GEOID" = cGEOID,
      "airToxins" = PercentPopScore,
    )
  ## censustract 
  censusTractScores <- allScores |> 
    dplyr::group_by(ctGEOID)|>
    dplyr::summarise(PercentPopScore = sum(aggregatedPercentPopScore))|>
    dplyr::select(
      "GEOID" = ctGEOID,
      "airToxins" = PercentPopScore,
    )
  ## census block group 
  censusBlockGroupScores <- allScores |> 
    dplyr::group_by(bgGEOID)|>
    dplyr::summarise(PercentPopScore = sum(aggregatedPercentPopScore))|>
    dplyr::select(
      "GEOID" = bgGEOID,
      "airToxins" = PercentPopScore,
    )
  return(
    list(
      "county" = countyScores,
      "censusTract" = censusTractScores,
      "censusBlockGroup" = censusBlockGroupScores
    )
  )

}




#' Generate airQuality measure
#'
#' @param filePath : location of airQuality raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getAir <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  d1 <- readxl::read_xlsx(path = filePath)
  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/airQuality")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- processAir(data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/airQuality_", name , ".csv"))
  }
}
