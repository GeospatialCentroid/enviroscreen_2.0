library(sf)
library(dplyr)
library(readr)
geometries <- processGeometryLayers()

files <- list.files("data/products/environmentalEffects/mining",
                    pattern = '.csv',
                    full.names = TRUE,
                    recursive = TRUE)
# select results files 
countyMine <- read_csv(file = files[5])
trackMine <- read_csv(file = files[4])
cbgMine <- read_csv(file = files[3])

# county layer 
g1 <- geometries$county |>
  dplyr::left_join(y = countyMine,by = c("GEOID" = "cGEOID"))
sf::st_write(obj = g1, dsn = "temp/mine_county.gpkg")
# county layer 
g2 <- geometries$censusTract |>
  dplyr::left_join(y = trackMine,by = c("GEOID" = "ctGEOID"))
sf::st_write(obj = g2, dsn = "temp/mine_tracts.gpkg")
# county layer 
g3 <- geometries$censusBlockGroup |>
  dplyr::left_join(y = cbgMine,by = c("GEOID" = "bgGEOID"))
sf::st_write(obj = g3, dsn = "temp/mine_cbg.gpkg")
