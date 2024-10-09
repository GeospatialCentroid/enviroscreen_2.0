### 
# two function for each indicators
# data processing function 
# function to call within the main script 
###

# 
# filePath <- "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv"
# data <- d1
# geometryLayers <- geometries
# geometry <- geometryFiles[1]

processlifeExectancy <- function(data){
  # this is a terrible data format... doing some adjustments
  coData <- data |>
    dplyr::filter(State == "Colorado") |>
    dplyr::filter(County != "(blank)") |> 
    dplyr::mutate(county = stringr::str_replace_all(string = County ,
                                                    pattern = " County, CO",
                                                    replacement = "")) |>
    #               # "Census Tract Number" = sprintf("%.2f", as.numeric(`Census.Tract.Number`)))|>
    # "Census Tract Number" = as.character(`Census.Tract.Number`))|>
    dplyr::select(
      "State",
      "county",
      "Census.Tract.Number",
      "Life.Expectancy", "Life.Expectancy.Standard.Error" 
    ) |>
    dplyr::mutate(
      ctn = as.character(Census.Tract.Number),
      exactCTN = sprintf("%.2f", as.numeric(`Census.Tract.Number`))
    )
  
  # construct the FIPS based GEOID 
  county <- sf::st_read("data/processed/geographies/county.gpkg")
  censusBlockGroup <- sf::st_read("data/processed/geographies/censusBlockGroup.gpkg")
  censusTract <- sf::st_read("data/raw/censusTract.gpkg")|>
    st_drop_geometry()|>
    dplyr::select(
      "STATEFP",
      "COUNTYFP",
      "TRACTCE",  
      "GEOID",
      "NAME",
      "NAMELSAD",
      "INTPTLAT", 
      "INTPTLON"
    )|>
    sf::st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = 4269)
  # 2010 data
  censusTract2010 <- sf::st_read("data/raw/censusTract2010.gpkg")|>
    dplyr::select("STATEFP10",
                  "COUNTYFP10",
                  "TRACTCE10",
                  "GEOID10",
                  "NAME10",
                  "NAMELSAD10")
  # intersect the 2020 centroids with the 2010 areas to get the 2010 GEOID 
  d1 <- sf::st_intersects(x = censusTract, y = censusTract2010,sparse = TRUE) |> unlist()
  # add the column to the 2020 dataset 
  censusTract$GEOID2010 <- censusTract2010$GEOID10[d1]
  # drop spatial elements 
  censusTract2010 <- st_drop_geometry(censusTract2010)
  censusTract <- st_drop_geometry(censusTract)
  
  
  # the census tract id here is 
  lifeData <- dplyr::left_join(x = coData, 
                               y = county,
                               by = c("county"="NAME"))|>
    dplyr::mutate(countyGEOID = str_sub(string = GEOID, start = 3,end = 5))
  
  lifeData$ctGEOID <- NA
  # itorate through the options and assing a censustract ID 
  for(i in seq_along(lifeData$Census.Tract.Number)){
    print(i)
    t1 <- lifeData[i, ]
    cID <- t1$countyGEOID
    # exact value 
    tID <- t1$exactCTN
    # shorterned value 
    sID <- t1$ctn
    
    # filter census tract data to count 
    ct1 <- dplyr::filter(.data = censusTract2010, COUNTYFP10 == cID)
    # test for exact match on
    ct2 <- dplyr::filter(ct1, NAME10 == tID)
    if(nrow(ct2)==1){
      lifeData[i, "ctGEOID"] <- ct2$GEOID10 
    }else{
      ct3 <- dplyr::filter(ct1, NAME10 == sID)
      if(nrow(ct3)==1){
        lifeData[i, "ctGEOID"] <- ct3$GEOID10 
      }
    }
  }
  
  # convert to 2020 censustract data 
  ## attempt a join 
  ct2022 <- dplyr::left_join(censusTract, lifeData, by = c("GEOID2010"= "ctGEOID"), keep = TRUE)
  
  finalVals <- ct2022 |>
    dplyr::select(
      "ctGEOID" = GEOID.x, 
      "Life.Expectancy")|>
    dplyr::mutate(
      cGEOID = stringr::str_sub(ctGEOID, start = 1, end = 5)) 

  # county
  countyVals <- finalVals |>
    dplyr::group_by(cGEOID)|>
    dplyr::summarise(lifeExpectancy = mean(Life.Expectancy, na.rm = TRUE))|>
    dplyr::select("GEOID" = cGEOID, lifeExpectancy)
  # census tract 
  ctVals <- finalVals |>
    dplyr::select("GEOID" = ctGEOID, 
                  lifeExpectancy = Life.Expectancy)
  # census blocks 
  cbgVals <- censusBlockGroup |>
    st_drop_geometry()|>
    dplyr::mutate(GEOID = stringr::str_sub(GEOID, start = 1, end = 11))|>
    dplyr::left_join(finalVals, by = c("GEOID" = "ctGEOID"))|>
    dplyr::select("GEOID",
                  lifeExpectancy = Life.Expectancy)
  
  
  return(
    list(
      "county" = countyVals,
      "censusTract" = ctVals,
      "censusBlockGroup" = cbgVals
    )
  )
}



### set parameters for testings 
filePath <- "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv"
# geometryLayers <- geometries

getLifeExpectency <- function(filePath, geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  d1 <- read.csv(filePath)
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/lifeExectancy")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  
  # render the results at the given spatial scales 
  results <- processlifeExectancy(data = d1)
  # export those results
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/lifeExectancy_", name , ".csv"))
  }
  
  #output the object
  return(results)
}

