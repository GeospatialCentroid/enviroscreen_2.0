###
geography <- "censusBlockGroup"
geoid <- "081010036001" 
generateDifference <- function(geography, geoid){
  # current run 
  currentRun <- paste0("data/products/enviroscreenScore/EnviroScreen_",geography,".csv") |>
    readr::read_csv()
  # original run 
  originalRun <- paste0("data/run1Products20241015/enviroscreenScore//EnviroScreen_",geography,".csv")|>
    readr:: read_csv()

  # select out the specific geoid 
  r1 <- currentRun[currentRun$GEOID == geoid,]
  r2 <- originalRun[originalRun$GEOID == geoid,]
  # bind and then take the difference 
  r3 <- dplyr::bind_rows(r1, r2)|>
    dplyr::mutate(run = c("current", "original"))
  r3[3,"GEOID"] <- r3$GEOID[1] 
  r3[3,"run"] <- "difference" 
  r3[3,3:90] <- r3[1,3:90] - r3[2,3:90]
  write_csv(x = r3, file = paste0("scripts/outputs/comparison_",geography,"_",geoid,".csv"))
}

generateDifference(geography = "censusBlockGroup",
                   geoid = "081010036001" )

# testing for zero and NA values within indicators 
# current run 
currentRun <- paste0("data/products/enviroscreenScore/EnviroScreen_censusBlockGroup.csv") |>
  readr::read_csv()

dfNA <- data.frame(matrix(nrow = 1, ncol = 89))
names(dfNA) <- names(currentRun)[2:90]
dfZero <- data.frame(matrix(nrow = 1, ncol = 89))
names(dfZero) <- names(currentRun)[2:90]

for(i in names(currentRun)[3:90]){
  print(i)
  d2 <- currentRun |> 
    dplyr::select(i)|>
    pull()
  # test for NA
  d3<- d2 |>
    is.na()|>
    unique()
  # test for zero
  d4 <- d2 == 0  
  d5 <- unique(d4)
  if(TRUE %in% d3){
    dfNA[1,i] <- TRUE
  }else{
    dfNA[1,i] <- FALSE
    
  }
  if(TRUE %in% d5){
    dfZero[1,i] <- TRUE
  }else{
    dfZero[1,i] <- FALSE
  }
}


