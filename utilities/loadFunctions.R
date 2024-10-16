#' loadFunctions 
#' @description : help function to load all functions. Useful when making edits as you can run in console 

loadFunctions <- function(path){
  # builds a path to the folder where .R scripts are contained 
  filePaths <- list.files(paste0(path,"/"),
                          pattern = ".R",
                          full.names = TRUE,
                          recursive = TRUE)
  # this grabs all files, so might hit some issues as we can only source .R files 
  # Loop here to print the function name, can be helpful to have
  for (i in seq_along(filePaths)) {
    print(filePaths[i])
    source(file = filePaths[i], echo = FALSE)
  }
}
