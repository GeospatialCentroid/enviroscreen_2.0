
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
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processlifeExectancy,
                         lifeExectancyLayer = d1)
  # export those results
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/noise_", name , ".csv"))
  }
  
  #output the object
  return(results)
}

