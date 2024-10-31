# 
# data <- allData
# geometry <- geometryFiles[[3]]
# name <- names(geometryFiles)[[3]]

processDrinkingWater <- function(geometry, name, data){
  # rename id variable 
  allPWS <-data$allPWS |>
    rename(PWS.ID = `PWS ID (Links to Records)`)
  
  # removing water haulers, keeping only Community systems, and calculating pop served per county
  county_popserved <- allPWS |>
    filter(`Regulated As Water Hauler?` == "No", 
           `Federal Type Full Name` == "Community")|>
    group_by(County) |>
    summarise(PopServed = sum(Population))
  
  #violation data 
  v.2013.2023.duration <- data$violations |>
    mutate(
      Begin.Date = as.Date(`Begin Date`, format = "%d-%b-%y"),
      End.Date = as.Date(`End Date`, format = "%d-%b-%y"),
      Year = as.numeric(format(Begin.Date, "%Y")),
      Unresolved = ifelse(Resolved == "No", 1, 0),
      Duration = as.numeric(difftime(End.Date, Begin.Date, units = "weeks")) / 52.25
    ) |>
    filter(Year < 2024, 
           Year > 2012,
           `Violation Name` %in% c(
             "EXCEEDED THE MAXIMUM CONTAMINANT LEVEL",
             "STATE FLUORIDE MCL",
             "MCL (TCR), ACUTE",
             "MCL (TCR), MONTHLY",
             "MRDL, ACUTE (CHL.DIOXIDE)",
             "STATE TREATMENT TECHNIQUE LEVEL"
           )
    ) |> 
    dplyr::select(
      PWS.ID =  "PWS ID", 
      Name, 
      Violation.Name = "Violation Name", 
      Analyte.Name = "Analyte Name" , 
      Unresolved, 
      County, 
      Year, 
      Duration, 
      Begin.Date, 
      End.Date)
  
  
  ### lead data 
  
  Pb90.duration <- data$lead %>%
    rename(Collection.Date = "Collection Date (Date & Time)") %>%
    mutate(DateNew = dmy(Collection.Date),
           Year = year(DateNew),
           AboveActionLevel = ifelse(Measure > 15, 1, 0),  
           Violation.Name = "Lead 90th Percentile",
           Analyte.Name = "Lead")%>%
    filter(Year < 2024,
           Year > 2012)%>%
    arrange(DateNew) %>%
    group_by(`PWS ID`) %>%
    mutate(Unresolved = case_when(
      DateNew == max(DateNew) & AboveActionLevel == 1 ~ 1,
      AboveActionLevel == 0 ~ NA_real_,
      TRUE ~ 0
    ),
    End.Date = lead(DateNew)) %>%
    ungroup()%>%
    group_by(`PWS ID`) %>%
    mutate(Duration = case_when(
      Unresolved == 1 ~ as.numeric(difftime(as.Date("12/31/2020", format = "%m/%d/%Y"), DateNew, units = "weeks"))/52.25, 
      Unresolved == 0 ~ as.numeric(difftime(lead(DateNew), DateNew,  units = "weeks"))/52.25,
      TRUE ~ NA_real_
    ))%>%
    ungroup()%>%
    filter(AboveActionLevel == 1) %>%
    select( PWS.ID =  "PWS ID", Name, Violation.Name, Analyte.Name, Unresolved, County, Year, Duration, Begin.Date = DateNew, End.Date)
  
  
  #### Combine violation datasets ----
  
  AllHBVs.d <- rbind(v.2013.2023.duration, Pb90.duration)
  
  # Trim leading and trailing spaces from PWS.ID in AllHBVs.d
  AllHBVs.d$PWS.ID <- trimws(AllHBVs.d$PWS.ID)
  
  # join to the data on districts 
  AllHBVs_pop.d <- merge(AllHBVs.d, allPWS, by = c("PWS.ID")) %>%
    select(PWS.ID, Name=Name.x, Violation.Name, Analyte.Name, Unresolved, Duration, County=County.x, Year, Population)

  # generate measure for the scores 
  System.Duration <- AllHBVs_pop.d %>%
    group_by(PWS.ID, Name, County) %>%
    summarise(ViolationCount = n(),
              Duration = sum(Duration),
              Population = mean(Population)) %>%
    ungroup()%>%
    mutate(numerator = Duration*Population)
  
  county.duration <- merge(System.Duration, county_popserved, by = "County", all = T)
  # assign a zero for all NA values  --- this needs to be declared in the documentation 
  county.duration[is.na(county.duration)] <- 0
  
  # summaize into final table 
  CountyViolations <- county.duration %>%
    group_by(County, PopServed) %>%
    summarise(ViolationCount = sum(ViolationCount),
              WeightedAverage = ifelse(ViolationCount > 0, sum(numerator)/PopServed, NA),
              TotalViolationYears = sum(Duration),
              AffectedPopulation = sum(Population))%>%
    group_by(County, ViolationCount, AffectedPopulation, TotalViolationYears, WeightedAverage) %>%
    summarise(PopServed = mean(PopServed)) %>%
    ungroup() %>%
    mutate(Percentile = percent_rank(WeightedAverage)*100,
           PercentPopulationAffected = 100*AffectedPopulation/PopServed) %>%
    select(County, PopServed, AffectedPopulation, PercentPopulationAffected, 
           ViolationCount, TotalViolationYears, WeightedAverage, Percentile)
  
  # join to county geometry to get the GEOID 
  countyGeom <- sf::st_read("data/processed/geographies/county.gpkg")
  
  # reformat names 
  CountyViolations$NAME <- stringr::str_to_title(string = CountyViolations$County)
  
  # join dataset and simplify 
  violationsGEOID <- dplyr::left_join(x = countyGeom, y = CountyViolations, by = c("NAME")) |>
    sf::st_drop_geometry()|>
    dplyr::select(-NAME,County)
  
  # process based on geography 
  if(name == "county"){
    
    # mortality data
    output <- violationsGEOID
  }
  # condition for census tract and census block group 
  if(name != "county"){
    # process down geometry  
    output <- geometry |>
      sf::st_drop_geometry()|>
      dplyr::mutate(
        geoid2 = stringr::str_sub(GEOID, start = 1, end = 5)
      )|>
      dplyr::left_join(y = violationsGEOID, by = c("geoid2" ="GEOID"))|>
      dplyr::select(-geoid2) 
  }
  
  # output
  return(output)
}




#' Generate drinkingWater measure
#'
#' @param filePath : location of drinkingWater raster data
#' @param geometryLayers : list of spatial object representing the processing levels
#' @return : a list of results at the three processing levels. -- think about if this is needed out not
#' 
getDrinkingWater <- function(geometryLayers){
  # select geometry layers of interest 
  geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
  # read in data 
  ## CDC data --- currently using age adjust prevalence 
  allPWS <- read_csv("data/raw/drinkingWater/AllPWS_CommunityNTNC_24_1_7.csv")
  lead <- read_csv("data/raw/drinkingWater/Lead90th_24_1_7.csv")
  violations <- read_csv("data/raw/drinkingWater/violations2013_2023.csv")

  # gather dataset for the function  
  allData <- list(
    allPWS = allPWS,
    lead = lead,
    violations = violations
  )
  # established the export 
  exportPathMain <- "data/products/environmentalExposures"
  # create export dir
  exportDir <- paste0(exportPathMain,"/drinkingWater")
  if(!dir.exists(exportDir)){
    dir.create(exportDir)
  }
  # process the datasets 
  results <- purrr::map2(.x = geometryFiles,
                         .y = names(geometryFiles),
                         .f = processDrinkingWater,
                         data = allData)
  
  for(i in seq_along(results)){
    data <- results[[i]]
    name <- names(results)[i]
    write.csv(x = data, file = paste0(exportDir,"/drinkingWater_", name , ".csv"))
  }
}
