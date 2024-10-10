
processAsthma <- function(geometry, name, data){
  
    # print statement to define process 
    print(paste0("Processing asthma data at the ", name, " geographic scale"))
    if(name == "county"){
      output <- data |> 
        dplyr::filter(geog == "County")|>
        dplyr::mutate(GEOID = paste0("0",geoid))|>
        dplyr::select(GEOID, "asthma" = adj_rate)
    }else{
      censustrack <- data |> 
        dplyr::filter(geog == "Census tract") |>
        dplyr::mutate(GEOID = paste0("0",geoid))|>
        dplyr::select(GEOID, adj_rate)
    }
    
    if(name == "censusTract"){
      output  <- censustrack |>
        dplyr::select(GEOID, "asthma" = adj_rate)
    }
    if(name == "censusBlockGroup"){
      # shorten the cbg geoid and join to the data at census level. 
      output <- st_drop_geometry(geometry) |>
        dplyr::mutate(GEOID2 = str_sub(GEOID, start = 1, end = 11)) |>
        dplyr::left_join(censustrack,  by = c("GEOID2" = "GEOID")) |>
        dplyr::select(GEOID, asthma = adj_rate)
    }
  
  # issues 
  
  return(output)
}




#' Generate Asthma measure
#'
#' @param filePath : location of Asthma raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getAsthma <- function(filePath,  geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  d1 <- read.csv(filePath) |>
    dplyr::filter(pop>0)|>
    dplyr::mutate(adj_rate = as.numeric(adj_rate))
  # established the export 
  exportPathMain <- "data/products/sensitivePopulation"
  # create export dir
  exportDir <- paste0(exportPathMain,"/asthma")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  ## in this case we need the names list as a reference for what geography is being used 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                        .f = processAsthma,
                        data = d1)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/asthma_", name , ".csv"))
  }
}



