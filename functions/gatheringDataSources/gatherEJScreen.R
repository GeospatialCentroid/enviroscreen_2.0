
# pull in the ejscreen data 
# library(googledrive)
## census block groups 


### move to a better location 
Force <- FALSE
if(Force == TRUE){
  googledrive::drive_download(file = as_id("https://drive.google.com/file/d/16ZK9gxpeug93EEgGyBiSsh3MH3STsr4Z/view?usp=drive_link"),
                              path = "data/raw/ejscreen/ejscreenDataBlocks.zip")
  #unzip
  unzip(zipfile = "data/raw/ejscreen/ejscreenDataBlocks.zip",exdir = "data/raw/ejscreen")
  
  ## census tracts
  googledrive::drive_download(file = as_id("https://drive.google.com/file/d/1am5SiLJidYQDsg6DWDMNrvvd6vo2xVhu/view?usp=drive_link"),
                              path = "data/raw/ejscreen/ejscreenDataTracts.zip")
  #unzip
  unzip(zipfile = "data/raw/ejscreen/ejscreenDataTracts.zip",exdir = "data/raw/ejscreen")
}



gatherEJScreen <- function(){
  # select specific indicators 
  ## Diesel particulate matter (PM),
  ## Fine particle pollution, 
  ## Traffic proximity and volume,
  ## proximity to hazardous waste facilities,
  ## Proximity to National Priorities List sites, 
  ## Proximity to Risk Management Plan (RMP)sites
  ## Wastewater discharge indicator
  # export data to specific geography 
  
  # Census Blocks -----------------------------------------------------------
  b1 <- vroom::vroom("data/raw/ejscreen/EJSCREEN_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv")
  b1 <- b1[b1$ST_ABBREV == "CO",]
  # grab data 
  b2 <- b1 |>
    as.data.frame()|>
    dplyr::select(
      GEOID = ID,
      particulateMatter = PM25,
      dieselPM = DSLPM,
      proxHazWaste = PTSDF,
      proxNPLsites = PNPL,
      proxRMPsites = PRMP,
      wasteWaterDischarge = PWDIS
    ) 
  #export 
  write.csv(b2, file = "data/processed/ejscreen/ejscreenBlocks.csv")
  
  
  # Census Tracts  ----------------------------------------------------------
  t1 <- vroom::vroom("data/raw/ejscreen/EJScreen_2024_Tract_StatePct_with_AS_CNMI_GU_VI.csv")
  t1 <- t1[t1$ST_ABBREV == "CO",]
  # grab data 
  t2 <- t1 |>
    as.data.frame()|>
    dplyr::select(
      GEOID = ID,
      particulateMatter = PM25,
      dieselPM = DSLPM,
      proxHazWaste = PTSDF,
      proxNPLsites = PNPL,
      proxRMPsites = PRMP,
      wasteWaterDischarge = PWDIS
    ) 
  #export 
  write.csv(t2, file = "data/processed/ejscreen/ejscreenTracts.csv")
  
  
  # counties ----------------------------------------------------------------
  ## take the mean of all values within a county based on the geoid of the tracts
  c1 <- t2 |>
    dplyr::mutate(GEOID = stringr::str_sub(GEOID, start = 1, end = 5))|>
    dplyr::group_by(GEOID)|>
    dplyr::summarise(
      particulateMatter = mean(particulateMatter,na.rm=TRUE),
      dieselPM =mean(dieselPM,na.rm=TRUE),
      proxHazWaste = mean(proxHazWaste,na.rm=TRUE),
      proxNPLsites = mean(proxNPLsites,na.rm=TRUE),
      proxRMPsites = mean(proxRMPsites,na.rm=TRUE),
      wasteWaterDischarge = mean(wasteWaterDischarge,na.rm=TRUE)
    )|>
    dplyr::filter(!is.na(GEOID))
  #export 
  write.csv(c1, file = "data/processed/ejscreen/ejscreenCounties.csv")
  
}
