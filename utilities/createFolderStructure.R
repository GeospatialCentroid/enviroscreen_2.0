#' Create Folder Structures
#'
#' This function creates a folder structure for data storage and organization.
#'
#' @return Nothing.
#'
#' @import dplyr

createFolderStructures <- function() {
  # Helper function to test for and create a directory
  testThenCreate <- function(path) {
    if (!dir.exists(path)) {
      dir.create(path)
      cat("Created directory:", path, "\n")
    } else {
      cat("Directory already exists:", path, "\n")
    }
  }
  
  # Set root directory
  root <- getwd()
  
  # Data directory
  dataPath <- paste0(root, "/data")
  testThenCreate(path = dataPath)
  
  # Subfolders within the data directory
  dataRaw <- paste0(dataPath, "/raw")
  dataProcessed <- paste0(dataPath, "/processed")
  dataProducts <- paste0(dataPath, "/products")
  primaryFolders <- c(dataRaw, dataProcessed, dataProducts)
  
  # Generate primary folders
  lapply(X = primaryFolders, FUN = testThenCreate)
  
  # Generate category folders within the products directory
  components <- c("environmentalExposures", "environmentalEffects", "climateVulnerability",
                  "sensitivePopulation", "demographics")
  productComponents <- paste0(dataProducts, "/", components)
  
  # Generate product component folders
  lapply(X = productComponents, FUN = testThenCreate)
  
  cat("All folders generated.\n")
}