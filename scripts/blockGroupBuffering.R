###
# run the geoprocessing to determine all BG within a 10km buffer of the a census block group 
# this list of Geoids will enable a quick reference for what block controids might need to be considered. 
###

library(sfdep)
bgs <- terra::vect("data/processed/geographies/censusBlockGroup.gpkg")

#' Buffer a block group and grap GEOID of all block groups within 10km 
#'
#' @param index : a numberical values based on the total number of block groups
#' @param allBlockGroups : a spatial data layer representing all block groups 
#'
#' @return
bufferAndCollect <- function(index, allBlockGroups){
  # ensure the input geometry is valid 
  validGeom <- terra::makeValid(allBlockGroups[index,])\
  # buffer the geomentry by 10000m.
  ## can use meters here because dataset is in an unprojected lat lon
  buf <- terra::buffer(x = validGeom, width = 10000)
  
  # crop 
  ## limit the number of outside block groups considered by cropping to the extent
  ## of the buffer object 
  cropBGS <- try(terra::crop(x = allBlockGroups, y = buf))
  
  # hitting errors with invalid geometries after the buffer. 
  # work around here was to take a centroid and buffer by an additional 2km so 12km
  # total. This should work as later spatail relationships are point to point in nature
  # This is just ment to determine what block groups should be considered. 
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
    return(ids) ## id's object is a vect of all census block groups within 10km of the 
    ## block group of interest. 
  }
}

# can't store the list inside the terra object so we create a DF 
bgsDF <- as.data.frame(bgs)
# using index for the function
vals <- seq_along(bgs)
# assign results 
output <- purrr::map(.x = vals, .f = bufferAndCollect, allBlockGroups = bgs)

# store the list object the data frame with the block group GEOID  
bgsDF$neighbors <- output

# export 
## this will be read in by later functions to determine what neighbor block groups
## should be considered. 
saveRDS(bgsDF, file = "data/processed/geographies/bgNeighbors.RDS")

