

#' Process geometry layers 
#' 
#' @description : grabs raw data from the `pullCensusGeographies` function and produces processed spatial data layers for all geographies of interest.
#' Each spatial data layer is writen to disk in the `processed/geographies` folder and is also returned as a named list by the function
#' @return : A name list containing the five spatial data layers used in this project. 
#' 
processGeometryLayers <-function(){
  # hard coded path to data location  
  paths <- list.files("data/raw",pattern = ".gpkg", full.names = TRUE)
  
  if(length(paths)== 0){
    print("No raw input data was found at the data/raw folder path. Ensure that the function `pullCensusGeographies` has been successfully ran.")
  }else{
  # create directory as needed
  exportDir <- "data/processed/geographies"
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  
  # grab the processing level name 
  geomName <- gsub(pattern =  ".gpkg",
                   replacement = "",
                   basename(paths))
  # define export paths 
  statePath <- paste0(exportDir,"/state.gpkg")
  countyPath <- paste0(exportDir,"/county.gpkg")
  censusTractPath <- paste0(exportDir,"/censusTract.gpkg")
  censusTractPath2010 <- paste0(exportDir,"/censusTract2010.gpkg")
  censusBlockGroupPath <- paste0(exportDir,"/censusBlockGroup.gpkg")
  censusBlockPath <- paste0(exportDir,"/censusBlock.gpkg")
  
  # state 
  print("processing state")
  if(!file.exists(statePath)){
    state <- sf::st_read(paths[grepl(pattern = "state", x = paths)]) |>
      dplyr::select(GEOID, NAME)
    sf::st_write(obj = state, dsn = statePath)
  }else{
    state <- sf::st_read(statePath)
  }
  # county 
  print("processing county")
  if(!file.exists(countyPath)){
    county <- sf::st_read(paths[grepl(pattern = "county", x = paths)])|>
      dplyr::select(GEOID, NAME)
    sf::st_write(obj = county, dsn = countyPath)
  }else{
    county <- sf::st_read(countyPath)
  }
  
  # censusTract
  print("processing census tract 2020")
  if(!file.exists(censusTractPath)){
    censusTract <- sf::st_read(paths[grepl(pattern = "censusTract.gpkg", x = paths)])|>
      dplyr::select(GEOID)
    sf::st_write(obj = censusTract, dsn = censusTractPath)
  }else{
    censusTract <- sf::st_read(censusTractPath)
  }
  
  # censusTract 2010
  print("processing census tract 2010")
  if(!file.exists(censusTractPath2010)){
    censusTract2010 <- sf::st_read(paths[grepl(pattern = "censusTract2010.gpkg", x = paths)])|>
      dplyr::select(GEOID10)
    sf::st_write(obj = censusTract, dsn = censusTractPath2010)
  }else{
    censusTract2010 <- sf::st_read(censusTractPath2010)
  }
  
  # census Block Groups 
  print("processing census block group")
  if(!file.exists(censusBlockGroupPath)){
    censusBlockGroups <- sf::st_read(paths[grepl(pattern ="censusBlockGroups", x = paths)])|>
      dplyr::select(GEOID)
    sf::st_write(obj = censusBlockGroups, dsn = censusBlockGroupPath)
  }else{
    censusBlockGroups <- sf::st_read(censusBlockGroupPath)
  }
  
  # census Blocks 
  print("processing census block")
  if(!file.exists(censusBlockPath)){
  censusBlocks <- sf::st_read(paths[grepl(pattern = "censusBlocks", x = paths)])|>
    dplyr::select(GEOID = GEOID20)
  sf::st_write(obj = censusBlocks, dsn =censusBlockPath)
  }else{
    censusBlocks <- sf::st_read(censusBlockPath)
  }
  
  # create a named list for storing objects 
  geomList <- list(
    state = state,
    county = county,
    censusTract = censusTract, 
    censusBlockGroup = censusBlockGroups,
    censusBlocks = censusBlocks      
  )
  return(geomList)
  
  }
}
