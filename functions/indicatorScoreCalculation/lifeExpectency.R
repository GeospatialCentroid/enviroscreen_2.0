### 
# two function for each indicators
# data processing function 
# function to call within the main script 
###

# 
filePath <- "data/raw/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015_20240703.csv"
geometryLayers <- geometries

geometry <- geometryFiles[1]

processlifeExectancy <- function(geometry, data){
  # this is a terrible data format... doing some adjustments
  coData <- data |>
    dplyr::filter(State == "Colorado") |>
    dplyr::filter(County != "(blank)") |> 
    dplyr::mutate(county = stringr::str_replace_all(string = County , pattern = " County, CO", replacement = ""))|>
    dplyr::select(
      "State","county", "Census.Tract.Number","Life.Expectancy", "Life.Expectancy.Standard.Error" 
    )
  
  name <- names(geometry)
  
  if(name == "county"){
    # read in county reference layer for GEOID
    countyRef <- sf::st_read("data/processed/geographies/county.gpkg")|>
      sf::st_drop_geometry()
    # filter the life expectence data and summarize 
    county <- coData |>
      dplyr::group_by(county)|>
      dplyr::summarise(aveLifeExpectancy = mean(Life.Expectancy, na.rm=TRUE))
    # join to reference layer 
    output <- countyRef |>
      dplyr::left_join(y = county, by = c("NAME" = "county"))
    return(list(
      name = output
      ))
  }
}

# file <- paste0(dir, "/",processingLevel,"_lifeExpectency.csv")
# if(file.exists(file) & isFALSE(overwrite)){
#   geom <- read_csv(file)
# }else{
#   g1 <- setSpatialData(processingLevel = "county")
#   
#   # read in data, filter to colorado, and separate county character column to facilitate join
#   d1 <- readr::read_csv(filePath) ## read r keeps the leading zeros
#   colnames(d1)[1] <- "State"
#   
#   d1 <- d1 %>%
#     dplyr::filter(`State` == "Colorado",!is.na(`Census Tract Number`))%>%
#     tidyr::separate(County, c("County", NA), sep = ",")%>%
#     # join on the county geom feature to grab GEOID to complete future joins
#     dplyr::left_join(y = g1, by = c("County" = "NAMELSAD"))%>%
#     # select significant columns.
#     dplyr::select(County, `Census Tract Number`,`Life Expectancy`,
#                   GEOID)%>%
#     dplyr::mutate(`Census Tract Number` = sprintf("%.2f", as.numeric(`Census Tract Number`)))
#   
#   # Life Exp data is at the census tract level, need to join data to CT geometry to get full GEOID
#   g2 <- setSpatialData(processingLevel = "censusTract")%>%
#     # create a second geoid to join on county level
#     dplyr::mutate(GEOID2 = stringr::str_sub(GEOID, 1,5),
#                   NAME = sprintf("%.2f", as.numeric(NAME)))
#   # Join data base on count match and census tract match, grab full geoid and output value
#   ## this is because census tract number is unqiue to county only
#   d1a <- dplyr::left_join(d1, g2, by= c("GEOID" = "GEOID2",
#                                         "Census Tract Number" = "NAME"))%>%
#     dplyr::select(GEOID = `GEOID.y`,`Life Expectancy`)
#   
#   
#   #The data structure hear matchs most other datasets, so apply similar process
#   ### select processing level by comparing length of GEOID between objects
#   if(nchar(geometry$GEOID[1]) >= nchar(d1a$GEOID[1])){
#     #add tract-level column to use as join then keep original geoid (tract or block)
#     geom <- st_drop_geometry(geometry) %>%
#       dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) %>%
#       dplyr::left_join(d1a, by =  c("GEOID2" = "GEOID")) %>%
#       dplyr::select(GEOID, lifeExpectancy = `Life Expectancy`)
#   }else{
#     # when geometry is county level.. just cut FIPS to county level and group by that
#     geom <-  d1a %>%
#       dplyr::mutate(GEOID = str_sub(GEOID, start = 1, end = 5))%>%
#       dplyr::group_by(GEOID) %>%
#       dplyr::summarise(lifeExpectancy = mean(`Life Expectancy`, na.rm = TRUE))
#   }
#   # replace all NaN with NA
#   geom$lifeExpectancy[is.nan(geom$lifeExpectancy)] <- NA
#   
#   write_csv(x = geom,file = file)
# }








### set parameters for testings 
# lifeExectancyLayer <- "data/lifeExpectancy/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015Colorado.csv"
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
  results <- purrr::map(.x = geometryFiles,
                         .f = processlifeExectancy,
                         data = d1)
  # export those results
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/noise_", name , ".csv"))
  }
  
  #output the object
  return(results)
}

