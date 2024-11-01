#' Load Functions from a Directory
#'
#' This function loads all R script files from a specified directory and sources them into the current environment.
#'
#' @param path The path to the directory containing the R script files.
#'
#' @return Nothing.

loadFunctions <- function(path) {
  # Get a list of all R script files in the directory and its subdirectories
  filePaths <- list.files(path,
                          pattern = ".R",
                          full.names = TRUE,
                          recursive = TRUE)
  
  # Source each R script file
  for (i in seq_along(filePaths)) {
    cat("Loading:", filePaths[i], "\n")
    source(file = filePaths[i], echo = FALSE)
  }
}
