###
# This method is designed to mirror the buffer approach from the 
# 2024 EJScreen technical documentation 
# --------------------------------------------
# Direct quotes from the ejscreen doc 
# 1. This formula is simply a population-weighted average – it sums the 
# population-weighted raw values, and then divides that sum by the total 
# population in the buffer.
#
# 2. To provide the most accurate counts that are currently feasible for a 
# screening tool, EJScreen uses an approach based on decennial Census block 
# internal points. EJScreen estimates the fraction of the Census block group 
# population that is inside the buffer by using block-level population counts 
# from the decennial Census
#
# --------------------------------------------
# Summary of methods from quotes 
#
# - buffer distance is a user defined features 
# - generate a buffer, and select all block internal points with are within the
#   buffer object
# - Calculate the total population of all blocks
# - using selected block, select the total population and measured value for each 
#   block group 
# - Calculations occur for each unique block group
# - Numerator : (pop of select Blocks / pop of block group decadial) * pop of block group acs * measured value of block group
# - denominator : sum of all (pop of select Blocks / pop of block group decadial) * pop of block group acs
# - Full value; sum of all records values within block groups 


# move this to the processing sections ------------------------------------
## need these from the raw folder for the population data and lat long of block centers
block <- terra::vect("data/raw/censusBlocks.gpkg") # pulled from 2020... 
blockGroups <- terra::vect("data/raw/censusBlockGroups.gpkg") 

library(tidycensus)
library(sf)

### need both the ACS estimate and the census value
# from the acs 
blockGroup_pop <- get_acs(
  cache_table = TRUE,
  geography = "cbg",
  year = 2022,
  state = "08",
  variables = "B01001_001"
)|>
  dplyr::select(GEOID,
                "estimateACSPop" = estimate)
## from the decadel --- my suggestion based on 1. alignment with ejscreen, 2. block level population data is from 2020
blockGroup_pop2 <- get_decennial(
  cache_table = TRUE,
  geography = "cbg",
  year = 2020,
  state = "08",
  variables = "P1_001N" # total number of people 
)|>
  dplyr::select(GEOID,
                "estimateCensusPop" = value)
# join to the blockgroup data
blockGroups2 <- blockGroups |>
  st_as_sf() |> 
  dplyr::left_join(y =blockGroup_pop, by = "GEOID")|>
  dplyr::left_join(y =blockGroup_pop2, by = "GEOID")|>
  terra::vect()



# generate the block points 
blockData <- as.data.frame(block) |>
  dplyr::mutate(
    lon = as.numeric(INTPTLON20),
    lat = as.numeric(stringr::str_sub(INTPTLAT20, 2, -2))
  )|>
  terra::vect(geom = c("lon","lat"), crs= terra::crs(block))

# generate the center of all blocks 
# terra::writeVector(x = blockData,filename = "data/processed/geographies/blockCenters.gpkg", overwrite=TRUE)

head(blockData)

calBufferValues <- function(location, bufferDist, measureValue, blockCenters, blockGroups){
  # location : the point or area of interest
  # measureValue : column name that is used to define the value of interest 
  # blockCenters : point dataset with pop values 
  # blockGroups : vector dataset with 
  
  # buffer location 
  buffArea <- terra::buffer(x = location, width = bufferDist)
  # test intersection on block centers
  t1 <- terra::intersect(x = blockCenters, y = buffArea)
  # select blocks of interest 
  b1 <- blockCenters[blockCenters$GEOID20 %in% t1$GEOID20, ]
  
  ## determine which census block groups are within the selected block centers 
  cbgID <- b1 |>
    as.data.frame()|>
    dplyr::mutate(
      bgGEOID = stringr::str_trunc(GEOID20, width = 12, side = "right",ellipsis ="")
    )|>
    dplyr::group_by(bgGEOID)|>
    dplyr::summarise(
      blockPop = sum(POP20, na.rm=TRUE)
    )
  # block group data with block values 
  selectCBG <- blockGroups |> 
    as.data.frame()|>
    dplyr::filter(GEOID %in% cbgID$bgGEOID)|> 
    dplyr::select("GEOID", "estimateACSPop", "estimateCensusPop")|>
    dplyr::left_join(y = cbgID, by = c("GEOID"= "bgGEOID")) |>
    dplyr::mutate(
      popWeighted = blockPop/estimateCensusPop* estimateACSPop,
      totalPop = sum(popWeighted),
      weightValue = popWeighted*measureValue,
      divideByTotalPop = weightValue / totalPop[1]
    )
  # find result 
  result <- sum(selectCBG$weightValue)/selectCBG$totalPop[1]
  ### this is just returning the measureValue, which makes sense because it's the only feature 
  ### not on the top and bottom of the equation 
  
  ### the other though here is that there might be a different measured value depending on the
  ### census block used. This may or may not apply to some buffered datasets, but wouldn't matter for 
  ### for things like apens 
  
  
}

