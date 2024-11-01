

removeZeroPopulation <- function(data, name){
  
  # get the column position to change 
  ## dangerous as were assuming it's the last column defined 
  val <- ncol(data)
  
  if(name == "censusTract"){
    # define zero pop area from ACS data 
    ct <- read_csv("data/processed/acs/acs_censusTract.csv") |>
      dplyr::filter(variable == "B01003_001") |>
      dplyr::filter(estimate == 0)|>
      dplyr::select(GEOID) |>
      pull()
    
    data[data$GEOID %in% ct ,val] <- NA
    return(data)
  }

  if(name == "censusBlockGroup"){
    # define zero pop area from ACS data 
    cbg <- read_csv("data/processed/acs/acs_censusBlockGroup.csv") |>
      dplyr::filter(variable == "B01003_001") |>
      dplyr::filter(estimate == 0)|>
      dplyr::select(GEOID) |>
      pull()

    
    data[data$GEOID %in% cbg ,val] <- NA
    return(data)
  }
}
