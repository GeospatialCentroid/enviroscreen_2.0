#' Calculate Distance Score for Emission Sites
#'
#' This function calculates a distance score for blocks of interest based on their proximity to an emission site.
#'
#' @param index The index of the current emission site within a loop.
#' @param sites A dataframe containing information about emission sites.
#' @param blockGroupNeighbors A dataframe relating census block groups to their expected neighbors.
#' @param blocks A dataframe containing information about all census blocks
#'
#' @return A dataframe containing selected blocks with their calculated distances, site score, distance score, non-population score, and percent population score.
#'
#' @import dplyr
#' @import sf

calculateDistanceScore <-
  function(index, sites, blockGroupNeighbors, blocks) {
    # Select the current emission site
    emissionSite <- sites[index,]
    
    # Print progress information (optional)
    if (index %% 25 == 0) {
      cat(paste0(index, " out of ", nrow(sites), "\n"))
    }
    
    # Identify expected neighboring block group IDs
    neighboringBlockGroupIDs <- blockGroupNeighbors |>
      filter(GEOID == emissionSite$cbg_geoid) |>
      pull(neighbors) |>
      unlist()
    
    # Filter blocks based on expected neighbors
    blockSelection <- blocks |>
      filter(bgGEOID %in% neighboringBlockGroupIDs) |>
      st_transform(crs(sites)) |>
      filter(acs2022PopAdj > 0)  # Optional: Remove blocks with zero population
    
    # Calculate distance to each block
    distance <- st_distance(x = emissionSite, y = blockSelection) |>
      as.data.frame() |>
      t()
    
    # Add distance and site score to block selection
    blockSelection$distance <-
      round(distance[, 1]) / 1000  # Convert distance to kilometers
    blockSelection$siteScore <- emissionSite$siteScore
    
    # Filter and score blocks based on distance
    blockSelectionDistanceFilter <- blockSelection |>
      filter(distance <= 5) |>  # Adjustable distance threshold
      mutate(
        adjDistance = case_when(distance < 0.1 ~ 0.1,
                                TRUE ~ distance),
        distanceScore = 1 / adjDistance,
        nonPopScore = siteScore * distanceScore,
        percentPopScore = siteScore * distanceScore * (percentOfCBGpopulation / 100)
      ) |>
      st_drop_geometry()
    
    return(blockSelectionDistanceFilter)
  }
