###
# run the geoprocessing to determine all BG within a 10km buffer of the a census block group 
# this list of Geoids will enable a quick reference for what block controids might need to be considered. 
###

library(sfdep)
bgs <- terra::vect("data/processed/geographies/censusBlockGroup.gpkg")


bufferAndCollect <- function(index, allBlockGroups){
  # buffer 
  validGeom <- terra::makeValid(allBlockGroups[index,])
  buf <- terra::buffer(x = validGeom, width = 10000)
  
  ### some issues with the buffered object on index 155 
  ### is is the work around, just test for error and buffer the point instead
  # crop 
  cropBGS <- try(terra::crop(x = allBlockGroups, y = buf))
  
  if(class(cropBGS) == "try-error"){
    centroid <- terra::centroids(validGeom)
    buf <- terra::buffer(x = centroid, width = 12000)
    cropBuff <- terra::crop(buf,y = terra::ext(allBlockGroups))
    cropBGS <- terra::crop(x = allBlockGroups, y = buf)
    # extract ids 
    ids <- list(cropBGS$GEOID)
    return(ids)
  }else{
    # extract ids 
    ids <- cropBGS$GEOID
    return(ids)
  }
}

# can't store the list inside the terra object so we create a DF 
bgsDF <- as.data.frame(bgs)
# using index for the function
vals <- seq_along(bgs)
# assign results 
output <- purrr::map(.x = vals, .f = bufferAndCollect, allBlockGroups = bgs)

bgsDF$neighbors <- output

# export 
saveRDS(bgsDF, file = "data/processed/geographies/bgNeighbors.RDS")

