#' createFolderStructures
#' @description
#' A helper function that generates a the folder structure for the project 
#' 
#' 
createFolderStructures <- function(){
  root <- getwd()
  # data 
  dataPath <- paste0(root,"/data")
  # test then create 
  if(!dir.exists(dataPath)){
    dir.create(dataPath)
  }else{
    print(paste0("The ", dataPath, " already exists."))
  }
  # subfolders within the data folder 
  dataRaw <- paste0(dataPath, "/raw")
  dataProcessed <- paste0(dataPath, "/processed")
  dataProducts <- paste0(dataPath, "/products")
  
  # test then create raw 
  if(!dir.exists(dataRaw)){
    dir.create(dataRaw)
  }else{
    print(paste0("The ", dataRaw, " already exists."))
  }
  
  # test then create data processed 
  if(!dir.exists(dataProcessed)){
    dir.create(dataProcessed)
  }else{
    print(paste0("The ", dataProcessed, " already exists."))
  }
  # test then create data products 
  if(!dir.exists(dataProducts)){
    dir.create(dataProducts)
  }else{
    print(paste0("The ", dataProducts, " already exists."))
  }
}