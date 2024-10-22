#' Processes life expectancy data and joins it with geographic information.
#'
#' This function takes a raw life expectancy dataset and performs several steps to prepare it for analysis.
#' It filters the data for Colorado, cleans county names, calculates census tract numbers, joins with geographic data,
#' and creates dataframes at different geographic levels (county, census tract, census block group).
#'
#' @param data A data frame containing raw life expectancy data.
#' @return A list containing three dataframes: county, censusTract, and censusBlockGroup.
processlifeExectancy <- function(data) {
  # Filter the data for Colorado and remove rows with missing county names
  coData <- data |>
    dplyr::filter(State == "Colorado", County != "(blank)") |>
    # Clean county names by removing " County, CO"
    dplyr::mutate(county = stringr::str_replace_all(string = County,
                                                    pattern = " County, CO",
                                                    replacement = "")) |>
    dplyr::select(
      "State",
      "county",
      "Census.Tract.Number",
      "Life.Expectancy", "Life.Expectancy.Standard.Error"
    ) |>
    # Create additional columns for census tract numbers
    dplyr::mutate(
      ctn = as.character(Census.Tract.Number),
      exactCTN = sprintf("%.2f", as.numeric(`Census.Tract.Number`))
    )
  
  # Load geographic data
  county <- sf::st_read("data/processed/geographies/county.gpkg")
  censusBlockGroup <- sf::st_read("data/processed/geographies/censusBlockGroup.gpkg")
  # pulling from raw for centroid info to test relationship with the 2010 ct data 
  censusTract <- sf::st_read("data/raw/censusTract.gpkg") |>
    st_drop_geometry() |>
    dplyr::select(
      "STATEFP",
      "COUNTYFP",
      "TRACTCE",
      "GEOID",
      "NAME",
      "NAMELSAD",
      "INTPTLAT",
      "INTPTLON"
    ) |>
    sf::st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = 4269)
  
  # Load 2010 census tract data
  censusTract2010 <- sf::st_read("data/raw/censusTract2010.gpkg") |>
    dplyr::select("STATEFP10",
                  "COUNTYFP10",
                  "TRACTCE10",
                  "GEOID10",
                  "NAME10",
                  "NAMELSAD10")
  
  # Intersect 2020 centroids with 2010 areas to get 2010 GEOID
  d1 <- sf::st_intersects(x = censusTract, y = censusTract2010, sparse = TRUE) |> unlist()
  
  # Add 2010 GEOID to 2020 dataset
  censusTract$GEOID2010 <- censusTract2010$GEOID10[d1]
  
  # Drop spatial elements from 2010 census tract data
  censusTract2010 <- st_drop_geometry(censusTract2010)
  censusTract <- st_drop_geometry(censusTract)
  
  # Join life data with county data
  lifeData <- dplyr::left_join(x = coData,
                               y = county,
                               by = c("county" = "NAME")) |>
    dplyr::mutate(countyGEOID = str_sub(string = GEOID, start = 3, end = 5))
  
  # Initialize ctGEOID column
  lifeData$ctGEOID <- NA
  
  # Iterate through life data and assign census tract GEOID
  for (i in seq_along(lifeData$Census.Tract.Number)) {
    t1 <- lifeData[i, ]
    cID <- t1$countyGEOID
    tID <- t1$exactCTN
    sID <- t1$ctn
    
    # Filter census tract data by county
    ct1 <- dplyr::filter(.data = censusTract2010, COUNTYFP10 == cID)
    
    # Find matching census tract by exact or shortened name
    ct2 <- dplyr::filter(ct1, NAME10 == tID)
    if (nrow(ct2) == 1) {
      lifeData[i, "ctGEOID"] <- ct2$GEOID10
    } else {
      ct3 <- dplyr::filter(ct1, NAME10 == sID)
      if (nrow(ct3) == 1) {
        lifeData[i, "ctGEOID"] <- ct3$GEOID10
      }
    }
  }
  
  # Convert to 2020 census tract data
  ct2022 <- dplyr::left_join(censusTract, lifeData, by = c("GEOID2010" = "ctGEOID"), keep = TRUE)
  
  # Calculate life expectancy at different geographic levels
  finalVals <- ct2022 |>
    dplyr::select(
      "ctGEOID" = GEOID.x,
      "Life.Expectancy"
    ) |>
    dplyr::mutate(
      cGEOID = stringr::str_sub(ctGEOID, start = 1, end = 5)
    )
  
  countyVals <- finalVals |>
    dplyr::group_by(cGEOID) |>
    dplyr::summarise(lifeExpectancy = mean(Life.Expectancy, na.rm = TRUE)) |>
    dplyr::select("GEOID" = cGEOID, lifeExpectancy)
  
  ctVals <- finalVals |>
    dplyr::select("GEOID" = ctGEOID,
                  lifeExpectancy = Life.Expectancy)
  
  cbgVals <- censusBlockGroup |>
    st_drop_geometry() |>
    dplyr::mutate(GEOID2 = stringr::str_sub(GEOID, start = 1, end = 11)) |>
    dplyr::left_join(finalVals, by = c("GEOID2" = "ctGEOID")) |>
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

#' Calculates life expectancy at different geographic levels.
#'
#' This function takes a file path to raw life expectancy data and a list of geometry layers.
#' It processes the data using the `processlifeExectancy` function, exports the results to CSV files,
#' and returns the results as a list.
#'
#' @param filePath A character string specifying the file path to the raw life expectancy data.
#' @param geometryLayers A list of geometry layers to be used for joining with the data.
#' @return A list containing the results of the life expectancy calculations at different geographic levels.
getLifeExpectency <- function(filePath, geometryLayers) {
  # Select geometry layers of interest
  geometryFiles <- geometryLayers[c("county", "censusTract", "censusBlockGroup")]
  
  # Read in data
  d1 <- read.csv(filePath)
  
  # Establish export path and create export directory
  exportPathMain <- "data/products/sensitivePopulation"
  exportDir <- paste0(exportPathMain, "/lifeExectancy")
  dir.create(exportDir, showWarnings = FALSE)
  
  # Process life expectancy data
  results <- processlifeExectancy(data = d1)
  
  # Export results to CSV files
  for (i in seq_along(results)) {
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir, "/lifeExectancy_", name, ".csv"))
  }
  
  # Return results
  return(results)
}












# 
# 
# 
# 
# 
# # old ---------------------------------------------------------------------
# 
# 
# 
# 
# ### 
# # two function for each indicators
# # data processing function 
# # function to call within the main script 
# ###
# 
# # 
# # filePath <- "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv"
# # data <- d1
# # geometryLayers <- geometries
# # geometry <- geometryFiles[3]
# 
# processlifeExectancy <- function(data){
#   # this is a terrible data format... doing some adjustments
#   coData <- data |>
#     dplyr::filter(State == "Colorado") |>
#     dplyr::filter(County != "(blank)") |> 
#     dplyr::mutate(county = stringr::str_replace_all(string = County ,
#                                                     pattern = " County, CO",
#                                                     replacement = "")) |>
#     #               # "Census Tract Number" = sprintf("%.2f", as.numeric(`Census.Tract.Number`)))|>
#     # "Census Tract Number" = as.character(`Census.Tract.Number`))|>
#     dplyr::select(
#       "State",
#       "county",
#       "Census.Tract.Number",
#       "Life.Expectancy", "Life.Expectancy.Standard.Error" 
#     ) |>
#     dplyr::mutate(
#       ctn = as.character(Census.Tract.Number),
#       exactCTN = sprintf("%.2f", as.numeric(`Census.Tract.Number`))
#     )
#   
#   # construct the FIPS based GEOID 
#   county <- sf::st_read("data/processed/geographies/county.gpkg")
#   censusBlockGroup <- sf::st_read("data/processed/geographies/censusBlockGroup.gpkg")
#   censusTract <- sf::st_read("data/raw/censusTract.gpkg")|>
#     st_drop_geometry()|>
#     dplyr::select(
#       "STATEFP",
#       "COUNTYFP",
#       "TRACTCE",  
#       "GEOID",
#       "NAME",
#       "NAMELSAD",
#       "INTPTLAT", 
#       "INTPTLON"
#     )|>
#     sf::st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = 4269)
#   # 2010 data
#   censusTract2010 <- sf::st_read("data/raw/censusTract2010.gpkg")|>
#     dplyr::select("STATEFP10",
#                   "COUNTYFP10",
#                   "TRACTCE10",
#                   "GEOID10",
#                   "NAME10",
#                   "NAMELSAD10")
#   # intersect the 2020 centroids with the 2010 areas to get the 2010 GEOID 
#   d1 <- sf::st_intersects(x = censusTract, y = censusTract2010,sparse = TRUE) |> unlist()
#   # add the column to the 2020 dataset 
#   censusTract$GEOID2010 <- censusTract2010$GEOID10[d1]
#   # drop spatial elements 
#   censusTract2010 <- st_drop_geometry(censusTract2010)
#   censusTract <- st_drop_geometry(censusTract)
#   
#   
#   # the census tract id here is 
#   lifeData <- dplyr::left_join(x = coData, 
#                                y = county,
#                                by = c("county"="NAME"))|>
#     dplyr::mutate(countyGEOID = str_sub(string = GEOID, start = 3,end = 5))
#   
#   lifeData$ctGEOID <- NA
#   # itorate through the options and assing a censustract ID 
#   for(i in seq_along(lifeData$Census.Tract.Number)){
#     print(i)
#     t1 <- lifeData[i, ]
#     cID <- t1$countyGEOID
#     # exact value 
#     tID <- t1$exactCTN
#     # shorterned value 
#     sID <- t1$ctn
#     
#     # filter census tract data to count 
#     ct1 <- dplyr::filter(.data = censusTract2010, COUNTYFP10 == cID)
#     # test for exact match on
#     ct2 <- dplyr::filter(ct1, NAME10 == tID)
#     if(nrow(ct2)==1){
#       lifeData[i, "ctGEOID"] <- ct2$GEOID10 
#     }else{
#       ct3 <- dplyr::filter(ct1, NAME10 == sID)
#       if(nrow(ct3)==1){
#         lifeData[i, "ctGEOID"] <- ct3$GEOID10 
#       }
#     }
#   }
#   
#   # convert to 2020 censustract data 
#   ## attempt a join 
#   ct2022 <- dplyr::left_join(censusTract, lifeData, by = c("GEOID2010"= "ctGEOID"), keep = TRUE)
#   
#   finalVals <- ct2022 |>
#     dplyr::select(
#       "ctGEOID" = GEOID.x, 
#       "Life.Expectancy")|>
#     dplyr::mutate(
#       cGEOID = stringr::str_sub(ctGEOID, start = 1, end = 5)) 
# 
#   # county
#   countyVals <- finalVals |>
#     dplyr::group_by(cGEOID)|>
#     dplyr::summarise(lifeExpectancy = mean(Life.Expectancy, na.rm = TRUE))|>
#     dplyr::select("GEOID" = cGEOID, lifeExpectancy)
#   # census tract 
#   ctVals <- finalVals |>
#     dplyr::select("GEOID" = ctGEOID, 
#                   lifeExpectancy = Life.Expectancy)
#   # census blocks 
#   cbgVals <- censusBlockGroup |>
#     st_drop_geometry()|>
#     dplyr::mutate(GEOID = stringr::str_sub(GEOID, start = 1, end = 11))|>
#     dplyr::left_join(finalVals, by = c("GEOID" = "ctGEOID"))|>
#     dplyr::select("GEOID",
#                   lifeExpectancy = Life.Expectancy)
#   
#   
#   return(
#     list(
#       "county" = countyVals,
#       "censusTract" = ctVals,
#       "censusBlockGroup" = cbgVals
#     )
#   )
# }
# 
# 
# 
# ### set parameters for testings 
# filePath <- "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv"
# # geometryLayers <- geometries
# 
# getLifeExpectency <- function(filePath, geometryLayers){
#   # select geometry layers of interest 
#   geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
#   # read in data 
#   d1 <- read.csv(filePath)
#   # established the export 
#   exportPathMain <- "data/products/sensitivePopulation"
#   # create export dir
#   exportDir <- paste0(exportPathMain,"/lifeExectancy")
#   if(!dir.exists(exportDir)){
#     dir.create(exportDir)
#   }
#   
#   # render the results at the given spatial scales 
#   results <- processlifeExectancy(data = d1)
#   # export those results
#   for(i in seq_along(results)){
#     data <- results[[i]]
#     name <- names(results)[i]
#     write.csv(x = data, file = paste0(exportDir,"/lifeExectancy_", name , ".csv"))
#   }
#   
#   #output the object
#   return(results)
# }
# 
