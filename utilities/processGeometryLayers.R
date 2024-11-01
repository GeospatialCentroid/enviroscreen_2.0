#' Process Geometry Layers
#'
#' This function processes raw geospatial data files (GPKG format) and extracts
#' specific geometry layers (state, county, census tract, census block group, census block).
#'
#' @return A list containing the extracted geometry layers.

processGeometryLayers <- function() {
  # Get paths to all GPKG files in the "data/raw" directory
  paths <- list.files("data/raw", pattern = ".gpkg", full.names = TRUE)
  
  # Check if any GPKG files were found
  if (length(paths) == 0) {
    stop("No raw input data was found at the data/raw folder path.
         Ensure that the function `pullCensusGeographies` has been successfully ran.")
  }
  
  # Create the output directory if it doesn't exist
  exportDir <- "data/processed/geographies"
  dir.create(exportDir, showWarnings = FALSE)
  
  # Extract geometry layer names from file paths
  geomName <- gsub(pattern = ".gpkg", replacement = "", basename(paths))
  # print(geomName) # determine the values to use in the grepl index 

  # Define output file paths
  statePath <- paste0(exportDir, "/state.gpkg")
  countyPath <- paste0(exportDir, "/county.gpkg")
  censusTractPath <- paste0(exportDir, "/censusTract.gpkg")
  censusTractPath2010 <- paste0(exportDir, "/censusTract2010.gpkg")
  censusBlockGroupPath <- paste0(exportDir, "/censusBlockGroup.gpkg")
  censusBlockPath <- paste0(exportDir, "/censusBlock.gpkg")
  
  # Process state geometry
  if (!file.exists(statePath)) {
    cat("Processing state geometry...\n")
    state <- sf::st_read(paths[grepl(pattern = "state", x = paths)]) |>
      dplyr::select(GEOID, NAME)
    sf::st_write(obj = state, dsn = statePath)
  } else {
    cat("State geometry already exists...\n")
    state <- sf::st_read(statePath,quiet = TRUE)
  }
  
  # Process county geometry
  if (!file.exists(countyPath)) {
    cat("Processing county geometry...\n")
    county <- sf::st_read(paths[grepl(pattern = "county", x = paths)]) |>
      dplyr::select(GEOID, NAME)
    sf::st_write(obj = county, dsn = countyPath)
  } else {
    cat("County geometry already exists...\n")
    county <- sf::st_read(countyPath,quiet = TRUE)
  }
  
  # Process census tract 2020 geometry
  if (!file.exists(censusTractPath)) {
    cat("Processing census tract 2020 geometry...\n")
    censusTract <- sf::st_read(paths[grepl(pattern = "censusTract.gpkg", x = paths)]) |>
      dplyr::select(GEOID)
    sf::st_write(obj = censusTract, dsn = censusTractPath)
  } else {
    cat("Census tract 2020 geometry already exists...\n")
    censusTract <- sf::st_read(censusTractPath,quiet = TRUE)
  }
  
  # Process census tract 2010 geometry
  if (!file.exists(censusTractPath2010)) {
    cat("Processing census tract 2010 geometry...\n")
    censusTract2010 <- sf::st_read(paths[grepl(pattern = "censusTract2010.gpkg", x = paths)]) |>
      dplyr::select(GEOID10)
    sf::st_write(obj = censusTract2010, dsn = censusTractPath2010)
  } else {
    cat("Census tract 2010 geometry already exists...\n")
    censusTract2010 <- sf::st_read(censusTractPath2010,quiet = TRUE)
  }
  
  # Process census block group geometry
  if (!file.exists(censusBlockGroupPath)) {
    cat("Processing census block group geometry...\n")
    censusBlockGroups <- sf::st_read(paths[grepl(pattern = "censusBlockGroups", x = paths)]) |>
      dplyr::select(GEOID)
    sf::st_write(obj = censusBlockGroups, dsn = censusBlockGroupPath)
  } else {
    cat("Census block group geometry already exists...\n")
    censusBlockGroups <- sf::st_read(censusBlockGroupPath,quiet = TRUE)
  }
  
  # Process census block geometry
  if (!file.exists(censusBlockPath)) {
    cat("Processing census block geometry...\n")
    censusBlocks <- sf::st_read(paths[grepl(pattern = "censusBlocks", x = paths)]) |>
      dplyr::select(GEOID = GEOID20)
    sf::st_write(obj = censusBlocks, dsn = censusBlockPath)
  } else {
    cat("Census block geometry already exists...\n")
    censusBlocks <- sf::st_read(censusBlockPath,quiet = TRUE)
  }
  
  # Create a named list for storing objects
  geomList <- list(
    state = state,
    county = county,
    censusTract = censusTract,
    censusBlockGroup = censusBlockGroups,
    censusBlocks = censusBlocks
  )
  
  return(geomList)
}
