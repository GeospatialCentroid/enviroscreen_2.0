# 
# filePath <- "data/raw/cancer/CONUS_L50dBA_sumDay_exi.tif"
# data <- allData
# geometry <- geometryFiles[[3]]
# name <- names(geometryFiles)[[3]]

processCancer <- function(geometry, name, data){
  
  # Using the county level prevalance data for all grographies 
  d1 <- data$county |>
    dplyr::filter(Measure =="Cancer (non-skin) or melanoma among adults",
                  Data_Value_Type == "Age-adjusted prevalence" )|>
    dplyr::mutate(GEOID = paste0("0",LocationID))|>
    dplyr::select(
      GEOID,
      adj_rate_Prevalence = Data_Value
    )
  
  
  # process based on geography 
  if(name == "county"){

    # mortality data
    d2 <- data$mortality |>
      dplyr::filter(geog == "County")|>
      dplyr::mutate(
          GEOID = paste0("0", geoid),
          adj_rate = as.numeric(adj_rate)
      )|>
      dplyr::select(
        GEOID,
        "adj_rate_mortality" = adj_rate
      )
    
    # join datasets and normalize the distributions 
    output <- d1 |>
      dplyr::left_join(d2, by = "GEOID")|>
      dplyr::mutate(
        cancerPrevalence_pcntl = cume_dist(adj_rate_Prevalence)*100,
        cancerMortality_pcntl = cume_dist(adj_rate_mortality )*100
      ) |>
      dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","cancerPrevalence_pcntl", "cancerMortality_pcntl" )
  }
  # condition for census tract and census block group 
  if(name != "county"){
    # process the datasets 
    
    # mortality 
    d2 <- data$mortality |>
      dplyr::filter(geog == "Census tract")|>
      dplyr::mutate(
        GEOID = paste0("0", geoid),
        "adj_rate_mortality" = as.numeric(adj_rate)
      )|>
      dplyr::select(
        GEOID,
        "adj_rate_mortality"
      )
    # set county FIPS for join 
    d2$geoid2 <- stringr::str_sub(d2$GEOID, start = 1, end = 5)
    
    # join datasets 
    ## need to add a county GEOID to make the 
    output <- d2 |>
      dplyr::left_join(d1, by = c("geoid2" = "GEOID"))|>
      dplyr::mutate(
        cancerPrevalence_pcntl = cume_dist(adj_rate_Prevalence)*100,
        cancerMortality_pcntl = cume_dist(adj_rate_mortality )*100
      )|>
      dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","cancerPrevalence_pcntl", "cancerMortality_pcntl" )

    
    # assign output based on geography name 
    if(name == "censusBlockGroup"){
      geometry$geoid2 <- str_sub(string = geometry$GEOID, start = 1, end = 11)
      # join to output and reformat
      output <- geometry |>
        sf::st_drop_geometry()|>
        dplyr::left_join(y =  output, by = c("geoid2"= "GEOID"))|>
        dplyr::select("GEOID","adj_rate_Prevalence","adj_rate_mortality","cancerPrevalence_pcntl", "cancerMortality_pcntl" )
    }
  }

  # output
  output <- output |>
    dplyr::rowwise()|>
    dplyr::mutate(combinedCancer = mean(c(cancerPrevalence_pcntl,cancerMortality_pcntl)))|>
    dplyr::select("GEOID", "combinedCancer")
  
  return(output)
}




#' Generate cancer measure
#'
#' @param filePath : location of cancer raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getCancer <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  ## CDC data --- currently using age adjust prevalence 
  cdcTracts <- read.csv("data/raw/CDC_places/PLACES_Tracts_24_CO.csv")
  cdcCounty <- read.csv("data/raw/CDC_places/PLACES_County_24_CO.csv")
  
  ## state data 
  mortality <- read.csv("data/raw/mortalityData/co_malignantcancer_death_nosupp_1822.csv")

  # gather dataset for the function  
  allData <- list(
    tract = cdcTracts,
    county = cdcCounty,
    mortality = mortality
  )
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/cancer")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                        .y = names(geometryFiles),
                        .f = processCancer,
                        data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/cancer_", name , ".csv"))
  }
}
