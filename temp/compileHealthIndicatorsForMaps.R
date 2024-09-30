library(sf)
library(dplyr)

geometries <- processGeometryLayers()

files <- list.files("data/products/sensitivePopulation",
                     pattern = '.csv',
                     full.names = TRUE,
                     recursive = TRUE)
# select disease  of interest 
heart <- files[grepl(pattern = "heart", x = files)] 
cancer <- files[grepl(pattern = "cancer", x = files)]
diabetes <- files[grepl(pattern = "diabetes", x = files)]
all <- c(heart, cancer, diabetes)

# county 
county <- all[grepl(pattern = "county", x = all)] |> 
  lapply(read_csv) |> 
  bind_cols() |> 
  dplyr::select(GEOID = "GEOID...2",
                combinedHeart,
                combinedCancer,
                combinedDiabetes)

# county layer 
g1 <- geometries$county |>
  dplyr::left_join(y = county,by = "GEOID")
sf::st_write(obj = g1, dsn = "temp/health_county.gpkg")


# census tract  
tracts <- all[grepl(pattern = "censusTract", x = all)] |> 
  lapply(read_csv)
# issues with binding so doing a bunch of joins 
t1 <- geometries$censusTract |>
  dplyr::left_join(y = tracts[[1]],by = "GEOID")|>
  dplyr::left_join(y = tracts[[2]],by = "GEOID")|>
  dplyr::left_join(y = tracts[[3]],by = "GEOID")|> 
  dplyr::select(GEOID,
                combinedHeart,
                combinedCancer,
                combinedDiabetes)
sf::st_write(obj = t1, dsn = "temp/health_tracks.gpkg")


# census block groups   
cbg <- all[grepl(pattern = "censusBlockGroup", x = all)] |> 
  lapply(read_csv)
# issues with binding so doing a bunch of joins 
cbg1 <- geometries$censusBlockGroup |>
  dplyr::left_join(y = cbg[[1]],by = "GEOID")|>
  dplyr::left_join(y = cbg[[2]],by = "GEOID")|>
  dplyr::left_join(y = cbg[[3]],by = "GEOID")|> 
  dplyr::select(GEOID,
                combinedHeart,
                combinedCancer,
                combinedDiabetes)
sf::st_write(obj = cbg1, dsn = "temp/health_cbg.gpkg")
qtm(cbg1)
