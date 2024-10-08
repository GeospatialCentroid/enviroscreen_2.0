

# probably want to include a summarized MOE process when aggregating the census values 
# https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
## basically looks like a rmse error 

# options for geographies c("county", "tract", "block group","block")

### 2024-10-01
### going to ignore the MOE measures for now and just get the datasets generated. 

### temp name to keep things from run on load

getACS <- function(geometryLayers, overwrite){
  if(overwrite == TRUE){
    # select geometry layers of interest 
    geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
    
    # create export dir
    exportDir <- "data/processed/acs"
    if(!dir.exists(exportDir)){
      dir.create(exportDir)
    }
    # process the datasets 
    results <- purrr::map2(.x = geometryFiles,
                           .y = names(geometryFiles),
                           .f = pullACS)
    
    for(i in seq_along(results)){
      data <- results[[i]]
      name <- names(results)[i]
      write.csv(x = data, file = paste0(exportDir,"/acs_", name , ".csv"))
    }
  }else{
    print("Set overwrite to TRUE if you wish to regerenate the ACS files ")
  }

}



pullACS <- function(geometry, name){
  # conditional to assign geography based on tidy census data standards 
  if(name == "county"){
    geography = "county"
  }
  if(name == "censusTract"){
    geography = "tract"
  }
  if(name == "censusBlockGroup"){
    geography = "cbg"
  }
  
  # pull the datasets 
  acs <- tidycensus::get_acs(
    geography = geography,
    variables = c(
      # under 5
      "B01001_003",
      "B01001_027",
      # over 64
      paste0("B01001_0", 20:25),
      paste0("B01001_0", 44:49),
      #percent people of color
      "B03002_001",
      "B03002_003",
      #Percent low income
      "C17002_001",
      "C17002_008",
      #Percent linguistic isolation
      "C16002_001",
      "C16002_004",
      "C16002_007",
      "C16002_010",
      "C16002_013",
      #Percent less than high school education
      "B15002_001",
      paste0("B15002_00", 3:9),
      "B15002_010",
      paste0("B15002_0", 20:27),
      #Percent disability
      paste0("B18101_", c("001","004","007","010","013","016","019","023",
                          "026","029","032","035","038")),
      #total Population
      "B01003_001",
      # lead housing 
      "B25034_001", # total housing units 
      "B25034_009", # 1950-1959
      "B25034_010", # 1940-1949
      "B25034_011", # pre 1939
      # housing burden
      "B25070_001", # Total Renters
      "B25070_007", # 30 to 34.9%
      "B25070_008", # 35 to 39.9%
      "B25070_009", # 40 to 49.9%
      "B25070_010", # 50% or more
      "B25091_001", # total owner-occupied,
      # "B25003_002", # confirmation of previous var - total owner occupied,
      "B25091_008", # 30 to 34.9% - mortgaged
      "B25091_009", # 35 to 39.9% - mortgaged
      "B25091_010", # 40 to 49.9% - mortgaged
      "B25091_011", # 50% or more - mortgaged
      "B25091_019", # 30 to 34.9% - not mortgaged
      "B25091_020", # 35 to 39.9% - not mortgaged
      "B25091_021", # 40 to 49.9% - not mortgaged
      "B25091_022" # 50% or more - not mortgaged
    ),
    state = "08",
    year = 2022
  )

  return(acs)
}  
  