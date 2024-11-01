

geometryLayers <- geometries

geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
# read in data 
data <- list.files("data/products/environmentalEffects",
                   pattern = ".csv",
                   full.names = TRUE,
                   recursive = TRUE)
# organize for the function
data <- list(
  county = data[grepl(pattern = "county", x = data)],
  censusTract = data[grepl(pattern = "censusTract", x = data)],
  censusBlockGroup = data[grepl(pattern = "censusBlockGroup", x = data)]
)

name <-"censusTract"
geometry <- geometryFiles$censusTract
vals <- data[[grep(pattern = name, x = names(data))]]

# pull the GEOID from the geometry object 
geom <- geometry |>
  sf::st_drop_geometry()|>
  dplyr::select("GEOID")
# read in and join datasets 
for(i in 1:length(vals)){
  print(i)
  # read in data 
  d1 <- readr::read_csv(vals[i])
  # some dataset have a row number column... should change that in the export 
  if(ncol(d1) == 3){
    d1 <- d1[,2:3]
  }
  
  geom <- geom |>
    dplyr::left_join(y = d1, by = "GEOID")
}



output <- geom |>
  dplyr::select(
    "GEOID",
    "Proximity to hazardous waste facilities" = "proxHazWaste",
    "Proximity to mining locations" = "PercentPopScore.x",
    "Proximity to National Priorities List sites" = "proxNPLsites",
    "Proximity to oil and gas" = "PercentPopScore.y",  
    "Proximity to Risk Management Plan sites" = "proxRMPsites",       
    "Impaired streams and rivers" = "surfaceWater",        
    "Wastewater discharge" = "wasteWaterDischarge"
  )




names(output)
df <- output |>
  dplyr::select("GEOID")
for(i in names(output)){
  if(i == "GEOID"){
    next
  }
  # select the column of interest 
  d1 <- output |> 
    dplyr::select("GEOID", i) 
  # define the column name 
  colName <- paste0(i, "_pcntl") 
  
  # select data for excluded values NA
  d2 <- d1[is.na(d1[,2]), ] 
  if(nrow(d2 > 0)){
    d2[, colName] <- NA
  }
  # adjust d1
  d1 <- d1[!d1$GEOID %in% d2$GEOID,]
  
  # select all zero values 
  d3 <- d1[d1[,2]==0,]
  if(nrow(d3 > 0)){
    d3[, colName] <- 0
  }
  # adjust d1
  d1 <- d1[!d1$GEOID %in% d3$GEOID,]
  # calculate on quality values 
  d1[, colName] <- cume_dist(d1[,2])*100
  # combine values and export 
  bindData <- bind_rows(d2,d3)|>
    bind_rows(d1)
  df <- df |>
    dplyr::left_join(y = bindData, by = "GEOID")
}

# not 