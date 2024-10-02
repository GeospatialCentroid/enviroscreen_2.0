#' get EJ Screen data 
#'
#' @param geometryLayers - spatial data layers 
#' @param overwrite - True/False value required to rerun the analysis
#'
#' @return
getEJscreen <- function(geometryLayers, overwrite){
  if(overwrite == TRUE){
    # select geometry layers of interest 
    geometryFiles <- geometryLayers[c("county","censusTract","censusBlockGroup")]
    
    # create export dir
    exportDir <- "data/processed/ejscreen"
    if(!dir.exists(exportDir)){
      dir.create(exportDir)
    }
    # process the datasets 
    results <- purrr::map2(.x = geometryFiles,
                           .y = names(geometryFiles),
                           .f = pullEJscreen)
    
    for(i in seq_along(results)){
      data <- results[[i]]
      name <- names(results)[i]
      write.csv(x = data, file = paste0(exportDir,"/ejscreen_", name , ".csv"))
    }
  }else{
    print("Set overwrite to TRUE if you wish to regerenate the EJScreen files ")
  }
  
}



#' Process the EJscreen datasets
#'
#' @param geometry - spatial data layer of one of the three geometries of interest 
#' @param name - the name of the geography layer, used for file paths 
#'
#' @return a dataframe of values for a specific geographic area 
pullEJscreen <- function(geometry, name){
  # conditional to assign geography based on tidy census data standards 
  if(name == "censusBlockGroup"){
    # read in data
    d1 <- readr::read_csv("data/raw/ejscreen/EJSCREEN_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv")
  }
  if(name != "censusBlockGroup"){
    # read in data
    d1 <- readr::read_csv("data/raw/ejscreen/EJScreen_2024_Tract_StatePct_with_AS_CNMI_GU_VI.csv")
  }
  
  # pull the datasets and select indicators
  ejscreen <- d1 |> 
    dplyr::filter(ST_ABBREV == "CO")|>
    dplyr::select(
      GEOID = ID,
      particulateMatter = PM25,
      dieselPM = DSLPM,
      traffic = PTRAF,
      proxHazWaste = PTSDF,
      proxNPLsites = PNPL,
      proxRMPsites = PRMP,
      wasteWaterDischarge = PWDIS)
  
  
  if(name == "county"){
    # drop GEOID to county level then sum all values 
    ejscreen <- ejscreen |>
      dplyr::mutate(GEOID = str_sub(string = ejscreen$GEOID, start = 1, end = 5))|>
      dplyr::group_by(GEOID)|>
      dplyr::summarise_all(mean,na.rm=TRUE)
  }
    
  # return 
  return(ejscreen)
}  






