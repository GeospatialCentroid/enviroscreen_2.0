


#' createFolderStructures
#' @description
#' A helper function that generates a the folder structure for the project 
#' 
#' 
createFolderStructures <- function(){
  # helper function 
  testThenCreate <- function(path){
    # test then create raw 
    if(!dir.exists(path)){
      dir.create(path)
    }else{
      print(paste0("The ", path, " already exists."))
    }
  }
  
  # set root 
  root <- getwd()
  # data 
  dataPath <- paste0(root,"/data")
  # data folder 
  testThenCreate(path = dataPath)
  # subfolders within the data folder 
  dataRaw <- paste0(dataPath, "/raw")
  dataProcessed <- paste0(dataPath, "/processed")
  dataProducts <- paste0(dataPath, "/products")
  primaryFolder <- c(dataRaw, dataProcessed, dataProducts)
  # generate folders 
  lapply(X = primaryFolder, FUN = testThenCreate)
  
  # generate category within the products folder 
  components <- c("environmentalExposures", "environmentalEffects", "climateVulnerability",
                  "sensitivePopulation", "demographics")
  # produce the sub layers 
  productComponents <- paste0(dataProducts,"/", components)
  # generate folders 
  lapply(X = productComponents, FUN = testThenCreate)
  print("all folders generated") 
  
}