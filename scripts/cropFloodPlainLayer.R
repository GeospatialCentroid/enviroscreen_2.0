### 
# flood plain layer was provided at the country level, 
# cropping as a independent layer before strating the processing method
### 


library(terra)
# read in flood layer 
flood1 <- terra::vect("data/raw/floodplains/floodHazard.shp")
# read in state geom 
state <- terra::vect("data/processed/geographies/state.gpkg")

stateProj <- state |>
  terra::project(flood1)

# crop 
floodCrop <- terra::crop(x = flood1, y = state)
# export 