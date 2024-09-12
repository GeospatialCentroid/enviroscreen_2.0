###
# stand along script for generating the population adjust block centroids 
### 


# 2020 blocks with population values 
blocks  <- tigris::blocks(state = "08", year = 2020)|> 
  st_drop_geometry()|> 
  dplyr::mutate(
    cGEOID = stringr::str_sub(GEOID20, start = 1, end = 5),
    ctGEOID = stringr::str_sub(GEOID20, start = 1, end = 11),
    bgGEOID = stringr::str_sub(GEOID20, start = 1, end = 12))|> # assigning this value later 
  dplyr::select(
    "GEOID20",
    "INTPTLAT20",
    "INTPTLON20",
    "POP20",
    "cGEOID",
    "ctGEOID",
    "bgGEOID")
# 2020 block groups with population values 
## it's oddly challenging pulling total pop directly from decadal census 
## so going this route by adding together all block population values 
bg2020 <- blocks |>
  dplyr::group_by(bgGEOID)|>
  dplyr::summarise(totalPop2020 = sum(POP20, na.rm=TRUE))|>
  dplyr::select(bgGEOID, totalPop2020)

# 5-year acs block groups with population values 
bg2022  <- tidycensus::get_acs(geography = "cbg",
                                variables = c(
                                       #total Population
                                       "B01003_001"),
                                state = "08",
                                year = 2022) |>
  dplyr::select(
    bgGEOID= GEOID,
    totalPop2022 = estimate
  )


# join and mutate for adjusted population 
blockJoin <- blocks |> 
  dplyr::left_join(y = bg2020, by = "bgGEOID")|>
  dplyr::left_join(y = bg2022, by = "bgGEOID")|>
  dplyr::mutate(
    acs2022PopAdj = case_when(
      POP20 == 0 ~ 0,
      POP20 != 0 ~ round((POP20/totalPop2020)*totalPop2022))
    )

# export
write.csv(blockJoin, file = "data/processed/geographies/blocksWithAdjustedPop.csv")
## generate spatail object 
blockPopAbj <- blockJoin |>
  st_as_sf(coords = c("INTPTLON20","INTPTLAT20"))|>
  st_set_crs(4326)
sf::st_write(blockPopAbj,  "data/processed/geographies/blocksWithAdjustedPop.gpkg",delete_dsn = TRUE)



