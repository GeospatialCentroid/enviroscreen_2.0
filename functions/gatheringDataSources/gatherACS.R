

# probably want to include a summarized MOE process when aggregating the census values 
# https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
## basically looks like a rmse error 

# options for geographies c("county", "tract", "block group","block")

### 2024-10-01
### going to ignore the MOE measures for now and just get the datasets generated. 

### temp name to keep things from run on load

getACS <- function(geometryLayers, overwrite){
  if(overwrite == TRUE){
    # select geometry layers of interest 
    geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
    
    # create export dir
    exportDir <- "data/processed/acs"
    if(!dir.exists(exportDir)){
      dir.create(exportDir)
    }
    # process the datasets 
    results <- purrr::map2(.x = geometryFiles,
                           .y = names(geometryFiles),
                           .f = pullACS)
    
    for(i in seq_along(results)){
      data <- results[[i]]
      name <- names(results)[i]
      write.csv(x = data, file = paste0(exportDir,"/acs_", name , ".csv"))
    }
  }else{
    print("Set overwrite to TRUE if you wish to regerenate the ACS files ")
  }

}



pullACS <- function(geometry, name){
  # conditional to assign geography based on tidy census data standards 
  if(name == "county"){
    geography = "county"
  }
  if(name == "censusTract"){
    geography = "tract"
  }
  if(name == "censusBlockGroup"){
    geography = "cbg"
  }
  
  # pull the datasets 
  acs <- tidycensus::get_acs(
    geography = geography,
    variables = c(
      # under 5
      "B01001_003",
      "B01001_027",
      # over 64
      paste0("B01001_0", 20:25),
      paste0("B01001_0", 44:49),
      #percent people of color
      "B03002_001",
      "B03002_003",
      #Percent low income
      "C17002_001",
      "C17002_008",
      #Percent linguistic isolation
      "C16002_001",
      "C16002_004",
      "C16002_007",
      "C16002_010",
      "C16002_013",
      #Percent less than high school education
      "B15002_001",
      paste0("B15002_00", 3:9),
      "B15002_010",
      paste0("B15002_0", 20:27),
      #Percent disability
      paste0("B18101_", c("001","004","007","010","013","016","019","023",
                          "026","029","032","035","038")),
      #total Population
      "B01003_001",
      # housing burden
      "B25070_001", # Total Renters
      "B25070_007", # 30 to 34.9%
      "B25070_008", # 35 to 39.9%
      "B25070_009", # 40 to 49.9%
      "B25070_010", # 50% or more
      "B25091_001", # total owner-occupied,
      # "B25003_002", # confirmation of previous var - total owner occupied,
      "B25091_008", # 30 to 34.9% - mortgaged
      "B25091_009", # 35 to 39.9% - mortgaged
      "B25091_010", # 40 to 49.9% - mortgaged
      "B25091_011", # 50% or more - mortgaged
      "B25091_019", # 30 to 34.9% - not mortgaged
      "B25091_020", # 35 to 39.9% - not mortgaged
      "B25091_021", # 40 to 49.9% - not mortgaged
      "B25091_022" # 50% or more - not mortgaged
    ),
    state = "08",
    year = 2022
  )

  return(acs)
}  
  
#   # test for and remove any geographies where total populations are zero 
#   acsNoPop <- acs |>
#     tidyr::spread(key = variable, value = estimate) |>
#     dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE)), .by = GEOID) |>
#     dplyr::filter(B01003_001 == 0)
#   if(nrow(acsNoPop) > 0){
#     acs <- acs |>
#       dplyr::filter(!GEOID %in% acsNoPop$GEOID)
#     readr::write_csv(acsNoPop, file = paste0("data/products/acs/zeroPopulation_",geography,".csv"))
#   }
#   
#   
#   # temp -- just pulling the required parameters ----------------------------
#   under5 <- acs |>
#     dplyr::filter(variable %in% c("B01001_003", "B01001_027"))|>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       age_under5 = sum(estimate)) |>
#     dplyr::select("GEOID","NAME","age_under5")|>
#     dplyr::distinct()
#   # age_over64 
#   over64 <- acs |>
#     dplyr::filter(variable %in% c("B01001_020", "B01001_021"))|>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       age_over64 = sum(estimate)) |>
#     dplyr::select("GEOID","NAME","age_over64")|>
#     dplyr::distinct()
#   # percent minority 
#   ## "B03002_001" total (should match total Population)
#   ## "B03002_003" white alone 
#   percentMinority <- acs |>
#     dplyr::filter(variable %in% c("B03002_001", "B03002_003"))|>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       percentMinority = ((estimate[1]-estimate[2]) / estimate[1])*100) |>
#     dplyr::select("GEOID","NAME","percentMinority")|>
#     dplyr::distinct()
#   # percent lingistically isolated
#   percent_lingiso <- acs |>
#     dplyr::filter(variable %in% c("C16002_004", "C16002_007",
#                                   "C16002_010", "C16002_013", "C16002_001")) |>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       percentLinIso = (sum(estimate[2:5]) / estimate[1])*100)|>
#     dplyr::select("GEOID","NAME","percentLinIso")|>
#     dplyr::distinct()
#   # percent less then high school education 
#   ### not sure if the MOE for the prop function is a percentile or not? 
#   percent_lths <- acs |>
#     dplyr::filter(variable %in% c("B15002_003","B15002_004","B15002_005","B15002_006",
#                                   "B15002_007","B15002_008","B15002_009","B15002_010",
#                                   "B15002_020","B15002_021","B15002_022","B15002_023",
#                                   "B15002_024","B15002_025","B15002_026","B15002_027",
#                                   "B15002_001"))  |>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       percentLTHS = (sum(estimate[2:17]) / estimate[1])*100)|>
#     dplyr::select("GEOID","NAME","percentLTHS")|>
#     dplyr::distinct()|>
#     dplyr::filter(!is.na(percentLTHS_moe)) 
#   # percent disability 
#   percent_disability <- acs |>
#     dplyr::filter(variable %in% c("B18101_004","B18101_007",
#                                   "B18101_010","B18101_013",
#                                   "B18101_016","B18101_019",
#                                   "B18101_023","B18101_026",
#                                   "B18101_029", "B18101_032",
#                                   "B18101_035","B18101_038",
#                                   "B18101_001"))|>
#     group_by(GEOID)|>
#     dplyr::mutate(
#       percentDisability  = (sum(estimate[2:12]) / estimate[1])*100)|>
#     dplyr::select("GEOID","NAME","percentDisability")|>
#     dplyr::distinct() 
#   # total population
#   totalPop <- acs |>
#     dplyr::filter(variable == "B01003_001")|>
#     dplyr::select("GEOID",
#                   "NAME",
#                   totalPopulation = "estimate")|>
#     dplyr::distinct()
# }








