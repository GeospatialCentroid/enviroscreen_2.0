

calculateCumulativeDistance <- function(data){
  
  df <- data |>
    dplyr::select("GEOID")
  for(i in names(data)){
    if(i == "GEOID"){
      next
    }
    # select the column of interest 
    d1 <- data |> 
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
  return(df)
}
