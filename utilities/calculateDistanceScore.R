#' Calculate the distance weighted populated score per census block 
#'
#' @param index : a numerical vector used to select rows from the sites dataframe 
#' @param sites : a spatial data object with all point source emission locations
#' @param blockGroupNeighbors : a dataframe with indexed for what block to evaluated 
#' @param blocks : a spatial data object representation the block location 
#'
#' @return : a dataframe of block elements with a distance - populaiton weighted scores per the specific emission source  
calculateDistanceScore <- function(index, sites, blockGroupNeighbors, blocks){
  # select element of interest 
  site <- sites[index, ]
  if(index %% 25 == 0){
    print(paste0(index, " out of ", nrow(sites)))
  } 
  # gather the expected census block groups 
  neighborCBG <- blockGroupNeighbors |> 
    dplyr::filter(GEOID == site$cbg_geoid) |>
    dplyr::pull(neighbors)|>
    unlist()
  # filter the blocks based on expect neighbors 
  blockSelection <- blocks |>
    dplyr::filter(bgGEOID %in% neighborCBG) |>
    sf::st_transform(crs(sites))|>
    # drop any zero population locations 
    dplyr::filter(acs2022PopAdj > 0)
  # detemine the distance between emmision site and all blocks of interest 
  distance <- st_distance(x = site,
                          y = blockSelection) |>
    as.data.frame() |>
    t()
  # add distance measure to the selected blocks 
  ## want this in km rather than meters for the 1/distance measures
  blockSelection$distance <- round(distance[,1]) / 1000 
  # add the site score from the site 
  blockSelection$siteScore <- site$siteScore
  
  blockSelectionDistanceFilter <- blockSelection |>
    dplyr::filter(distance <= 5)|> # this might change 
    # add a measure for the very small distances, keeps the distance score to a max of 10 
    dplyr::mutate(adjDistance = case_when( 
      distance < 0.1 ~ 0.1,
      .default = distance),
      distanceScore = 1/adjDistance,
      nonPopScore = siteScore * distanceScore, # no population considered 
      percentPopScore = siteScore * distanceScore * (percentOfCBGpopulation/100) # converting back to vals between 0-1
    )|> st_drop_geometry()
  return(blockSelectionDistanceFilter)
}
