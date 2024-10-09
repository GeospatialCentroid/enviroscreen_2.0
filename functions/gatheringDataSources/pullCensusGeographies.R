
#' pullCensusGeographies
#'
#' @param overwrite : binary value used to determine if existing data should be overwritten 

#' @export : spatial data layers for the required geographies of Colorado for 2020 

pullCensusGeographies <- function(overwrite = FALSE){
  # define files 
  state <- "data/raw/state.gpkg"
  county <- "data/raw/county.gpkg"
  censusTract <- "data/raw/censusTract.gpkg"
  censusTract2010 <- "data/raw/censusTract2010.gpkg"
  censusBlockGroups <- "data/raw/censusBlockGroups.gpkg"
  censusBlocks <- "data/raw/censusBlocks.gpkg"
  
  # set internal functions for download and export 
  ## state 
  getState <- function(path){
    # download state 
    stateData <- tigris::states(year = 2020)|>
      dplyr::filter(GEOID == "08")
    # export 
    sf::st_write(stateData, dsn = path,delete_layer = TRUE,quiet = TRUE)
  }
  # county 
  getCounty <- function(path){
    # download state 
    countyData <- tigris::counties(state = "08", year = 2020)
    # export 
    sf::st_write(countyData, dsn = path, delete_layer = TRUE,quiet = TRUE)
  }
  # census tract 
  getcensusTract <- function(path){
    # download state 
    censusTractData <- tigris::tracts(state = "08", year = 2020)
    # export 
    sf::st_write(censusTractData, dsn = path, delete_layer = TRUE,quiet = TRUE)
  }
  # second census tract for life expectancy data 
  getcensusTract <- function(path){
    # download state 
    censusTractData <- tigris::tracts(state = "08", year = 2010)
    # export 
    sf::st_write(censusTractData, dsn = path, delete_layer = TRUE,quiet = TRUE)
  }
  
  
  # census Block Groups 
  getcensusBlockGroups <- function(path){
    # download state 
    censusBlockGroupsData <- tigris::block_groups(state = "08", year = 2020)
    # export 
    sf::st_write(censusBlockGroupsData, dsn = path, delete_layer = TRUE,quiet = TRUE)
  }
  # census Blocks 
  getcensusBlocks <- function(path){
    # download state 
    censusBlocksData <- tigris::blocks(state = "08", year = 2020)
    # export 
    sf::st_write(censusBlocksData, dsn = path, delete_layer = TRUE,quiet = TRUE )
  }
  
  # Test for the overwrite command 
  if(overwrite == TRUE){
    print("grabbing state")
    getState(path = state)
    print("grabbing county")
    getCounty(path = county)
    print("grabbing census tract")
    getcensusTract(path = censusTract)
    print("grabbing census tract 2010")
    getcensusTract(path = censusTract2010)
    print("grabbing census block group")
    getcensusBlockGroups(path = censusBlockGroups)
    print("grabbing census blocks")
    getcensusBlocks(path = censusBlocks)
  }else{
    # check for file then write
    #state
    if(!file.exists(state)){
      getState(path = state)
    }
    #county
    if(!file.exists(county)){
      getCounty(path = county)
    }
    #censusTract
    if(!file.exists(censusTract)){
      getcensusTract(path = censusTract)
    }
    #getcensusBlockGroups
    if(!file.exists(censusBlockGroups)){
      getcensusBlockGroups(path = censusBlockGroups)
    }
    #censusBlocks
    if(!file.exists(censusBlocks)){
      getcensusBlocks(path = censusBlocks)
    }
  }
}



# load in census blocks ---------------------------------------------------
## this dataset was download from tigis 2020 census 
## converted to a gpkg in QGIS 
## uploaded to drive, then download here
## drive link no longer exists, just including reference here for example. 
# library(googledrive)
# d1 <- googledrive::drive_download(file = googledrive::as_id("https://drive.google.com/file/d/1SjeoPLr0Go4UFTOwUXksZLekhVelkSFM/view?usp=drive_link"))
