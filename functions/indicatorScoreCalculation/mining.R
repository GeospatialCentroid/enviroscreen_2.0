# 
# filePath <- "data/raw/Mining/CONUS_L50dBA_sumDay_exi.tif"
# data <- terra::rast(filePath)  
# geometry <- geometryFiles[[1]]
processMining <- function(data, geometries){
  # read in reference layers 
  blocks <- sf::st_read("data/processed/geographies/blocksWithAdjustedPop.gpkg")
  # block group relations 
  blockGroupNeighbors <- readRDS("data/processed/geographies/bgNeighbors.RDS")
  print("reading in reference layers")
  # define the index for the map function
  index <- 1:nrow(data)
  # call the calculate score function 
  exportFile <-  "data/products/environmentalEffects/mining/detailsOnDistanceScoring.csv"
  ## conditional to avoid timely geoprocessing step 
  if(!file.exists(exportFile)){
    for(i in index){
      val <- calculateDistanceScore(index = i,
                                    sites = data, 
                                    blockGroupNeighbors= blockGroupNeighbors,
                                    blocks = blocks )
      if(i == 1){
        scores <- val
      }else{
        scores <- scores |> bind_rows(val)
      }
    }

  # export here because this is a big geoprocessing step 
    write.csv(scores, file = "data/products/environmentalEffects/mining/detailsOnDistanceScoring.csv")
    }else{
     scores <- readr::read_csv(exportFile)
  }

  
  formatedScores <- scores |> 
    # summarize to agggregate measures to the blockGEOID 
      dplyr::group_by(GEOID20)|>
      dplyr::summarise(aggregatedNoPopScore = sum(nonPopScore),
                       aggregatedPercentPopScore = sum(percentPopScore),
                       numberOfSource = n())
  write.csv(scores, file = "data/products/environmentalEffects/mining/aggratedScoreValues.csv")
  
  
  # group these by census block group, census tract, county 
  allScores <- formatedScores |> 
    dplyr::mutate(
      cGEOID = stringr::str_sub(GEOID20, start = 1, end = 5),
      ctGEOID = stringr::str_sub(GEOID20, start = 1, end = 11),
      bgGEOID = stringr::str_sub(GEOID20, start = 1, end = 12)
    )
  # write.csv(scores, file = "data/products/environmentalEffects/mining/mining_census.csv")
  # 
  # generate aggregates score measures 
  ## county
  countyScores <- allScores |> 
    dplyr::group_by(cGEOID)|>
    dplyr::summarise(noPopScore = sum(aggregatedNoPopScore),
                     PercentPopScore = sum(aggregatedPercentPopScore),
                     numberOfSource = n())|>
    dplyr::select(
      "GEOID" = cGEOID,
      noPopScore,
      PercentPopScore,
      numberOfSource
    )
  # join to get all missing features and assigned values of zero 
  countyScores <- geometries$county |>
    st_drop_geometry()|>
    dplyr::select("GEOID")|>
    dplyr::left_join(countyScores ,by = "GEOID")|>
    dplyr::mutate(PercentPopScore = case_when(is.na(PercentPopScore) ~ 0,
                                        .default = PercentPopScore))
  
  ## censustract 
  censusTractScores <- allScores |> 
    dplyr::group_by(ctGEOID)|>
    dplyr::summarise(noPopScore = sum(aggregatedNoPopScore),
                     PercentPopScore = sum(aggregatedPercentPopScore),
                     numberOfSource = n())|>
    dplyr::select(
      "GEOID" = ctGEOID,
      noPopScore,
      PercentPopScore,
      numberOfSource
    )
  # join to get all missing features and assigned values of zero 
  censusTractScores <- geometries$censusTract |>
    st_drop_geometry()|>
    dplyr::select("GEOID")|>
    dplyr::left_join(censusTractScores ,by = "GEOID")|>
    dplyr::mutate(PercentPopScore = case_when(is.na(PercentPopScore) ~ 0,
                                              .default = PercentPopScore))
  
  
  ## census block group 
  censusBlockGroupScores <- allScores |> 
    dplyr::group_by(bgGEOID)|>
    dplyr::summarise(noPopScore = sum(aggregatedNoPopScore),
                     PercentPopScore = sum(aggregatedPercentPopScore),
                     numberOfSource = n())|>
    dplyr::select(
      "GEOID" = bgGEOID,
      noPopScore,
      PercentPopScore,
      numberOfSource
    )
  # join to get all missing features and assigned values of zero 
  censusBlockGroupScores <- geometries$censusBlockGroup |>
    st_drop_geometry()|>
    dplyr::select("GEOID")|>
    dplyr::left_join(censusBlockGroupScores ,by = "GEOID")|>
    dplyr::mutate(PercentPopScore = case_when(is.na(PercentPopScore) ~ 0,
                                              .default = PercentPopScore))
  
  
  
  return(
    list(
      "county" = countyScores,
      "censusTract" = censusTractScores,
      "censusBlockGroup" = censusBlockGroupScores
    )
  )
}




#' Generate Mining measure
#'
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getMining <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  hardrock <- sf::st_read("data/raw/mining/Active_Hardrock_Permit/Active_Hardrock_Permit.shp")|>
    dplyr::select(PermitID, SiteName, StatusDesc, PermitType, Commodity, PermitAcre, Longitude, Latitude, MineType)|>
    dplyr::mutate(DataSource = "Hardrock")
  constr <- sf::st_read("data/raw/mining/Active_Construction_Permit/Active_Construction_Permit.shp")|>
    dplyr::select(PermitID, SiteName, StatusDesc, PermitType, Commodity, PermitAcre, Longitude, Latitude, MineType)|>
    dplyr::mutate(DataSource = "Construction")
  coal <- sf::st_read("data/raw/mining/Active_Coal_Permit/Active_Coal_Permit.shp")|>
    dplyr::select(PermitID, SiteName, StatusDesc, PermitType, PermitAcre, Longitude, Latitude, MineType)|>
    dplyr::mutate(Commodity = "Coal",
           DataSource = "Coal")
  
  allMining <- rbind(hardrock, constr, coal)|>
    dplyr::filter(StatusDesc == "Active") |> 
    sf::st_transform(crs = crs(geometryFiles[[1]]))
  
  # pull census block groups 
  cbg <- geometryFiles$censusBlockGroup
  # returns a postition index on the interestion of the cbg per each mining location
  t1 <- sf::st_intersects(x = allMining,
                          y = cbg,
                          sparse = TRUE
                          )
  # when I unlist() the t1 object I'm looking a row... not sure why so using a for loop to assign data
  ## item 84, has a lat value outside of the state so it can not reference cbg 
  allMining$cbg_geoid <- NA
  
  for(i in 1:length(t1)){
    index <- cbg$GEOID[t1[[i]]]
    if(identical(index, character(0))){
      allMining$cbg_geoid[i] <- NA
    }else{
      allMining$cbg_geoid[i] <- index
    }
  }
    
  # remove any na values 
  allMining_clean <- allMining |> dplyr::filter(!is.na(cbg_geoid))
  
  # define the site score value (different for each indicator. )
  allMining_clean$siteScore <- 1
  # established the export 
  exportPathMain <- "data/products/environmentalEffects"
  
  # create export dir
  exportDir <- paste0(exportPathMain,"/mining")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- processMining(data = allMining_clean, geometries=geometryFiles)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/mining_", name , ".csv"))
  }
}
