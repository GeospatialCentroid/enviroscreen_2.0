

if(!exists("data/products/enviroscreenScore/shp")){dir.create("data/products/enviroscreenScore/shp")}

# read in renamesets 
## enviroscreen rename 
data <- list.files(path = "data/products/enviroscreenScore",
                   pattern = ".csv",
                   full.names = TRUE) |> readr::read_csv()


## geographic rename 
geometries <- processGeometryLayers() 
county <- geometries$county
censusTract <- geometries$censusTract
cbg <- geometries$censusBlockGroup

## export a gpkp with human readable headers  
c1 <- county |>
  dplyr::left_join(y = data, by = "GEOID")

sf::st_write(c1,
             "data/products/enviroscreenScore/countyEnviroScreenScores.gpkg",
             delete_dsn = TRUE)

ct1 <- censusTract |>
  dplyr::left_join(y = data, by = "GEOID")

sf::st_write(ct1, 
             "data/products/enviroscreenScore/censusTractEnviroScreenScores.gpkg",
             delete_dsn = TRUE)


cbg1 <- cbg |>
  dplyr::left_join(y = data, by = "GEOID")

sf::st_write(cbg1,
             "data/products/enviroscreenScore/censusBlockGroupEnviroScreenScores.gpkg",
             delete_dsn = TRUE)


## export a shp with headers for app 
renamed <- data |>
  dplyr::select(
    "GEOID",
    "Hsn__" = "Housing cost burden",                              
    "H___P" = "Housing cost burden_pcntl", 
    "Prcn_"="Percent disability",                               
    "Pr__P"="Percent disability_pcntl",
    "Pr_____"="Percent less than high school education",
    "P______"="Percent less than high school education_pcntl",    
    "Prcnt_ln_"="Percent linguistic isolation",
    "Prcnt_ln__"="Percent linguistic isolation_pcntl",
    "Prcnt_lw_"="Percent low income",
    "Prcnt_lw__"="Percent low income_pcntl",
    "Prc___"="Percent people of color",
    "Prc____P"="Percent people of color_pcntl",                    
    "Dmg_S"="demograpics",
    "Ast__" ="Asthma",
    "As___P"="Asthma_pcntl",
    "Cncr_"="Cancer",
    "Cn__P"="Cancer_pcntl",
    "Dbts_"="Diabetes",                                         
    "Db__P"="Diabetes_pcntl",
    "Hr___"="Cadiovascular",
    "H____"="Cadiovascular_pcntl",                              
    "Lf_xp"="Life expectancy",
    "Lf__P"="Life expectancy_pcntl",
    "Lw_b_"="Low birth weight",                                 
    "Lw___P"="Low birth weight_pcntl",
    "Mnt__"="Mental health" ,
    "M___P"="Mental health_pcntl",                              
    "Pp__64"="Population over 64",
    "P__64_"="Population over 64_pcntl",
    "Pp__5"="Population under 5",                               
    "P__5_"="Population under 5_pcntl",
    "Sn_P_S"="sensitivePopulation"  ,
    "Hl__S_F_S"="popCharacteristic",                                
    "Ar_t_"="Air toxics emissions" ,
    "Ar___P"="Air toxics emissions_pcntl",                       
    "Ds____PM_"="Diesel particulate matter",
    "D____PM__"="Diesel particulate matter_pcntl" ,
    "Drn__"="Drinking water regulations",                       
    "D___P"="Drinking water regulations_pcntl",
    "Ld_x_"="Lead exposure risk"  ,
    "Ld___P"="Lead exposure risk_pcntl",                         
    "Noise"="Noise",
    "Ns_Pr"="Noise_pcntl",
    "Oth__"="Other air pollutants",                             
    "O___P"="Other air pollutants_pcntl",
    "Ozone" ="Ozone",
    "Ozn_P"="Ozone_pcntl",                                      
    "Fn_p_"="Fine particle pollution", 
    "F___P"="Fine particle pollution_pcntl",
    "Tr____"="Traffic proximity and volume",                     
    "T_____"="Traffic proximity and volume_pcntl",
    "Envrnmntl_"="environmentalExposures",        
    "Prxmty_t_h"="Proximity to hazardous waste facilities",          
    "Prxmty_t_1"="Proximity to hazardous waste facilities_pcntl",
    "Prx___"="Proximity to mining locations",           
    "Prx____P"="Proximity to mining locations_pcntl",              
    "Pr__N_P_L_"="Proximity to National Priorities List sites",
    "P__N_P_L__"="Proximity to National Priorities List sites_pcntl",
    "Prxmty_t_l"="Proximity to oil and gas",                         
    "Prxmty_t_2" ="Proximity to oil and gas_pcntl",
    "Pr__R_M_P_"="Proximity to Risk Management Plan sites",
    "P__R_M_P__"="Proximity to Risk Management Plan sites_pcntl",    
    "Im___"="Impaired streams and rivers", 
    "I____"="Impaired streams and rivers_pcntl",
    "Wst__"="Wastewater discharge",                             
    "W___P"="Wastewater discharge_pcntl",
    "Envrnmnt_1"="environmentalEffects", 
    "Drght"="Drought",                                          
    "Drg_P"="Drought_pcntl",     
    "Fldpl"="Floodplains",
    "Fld_P"="Floodplains_pcntl",                                
    "Ext__"="Extreme heat days",  
    "E___P"="Extreme heat days_pcntl",  
    "Wldf_"="Wildfire risk",                                    
    "Wl__P"="Wildfire risk_pcntl",   
    "Cl_V_S"="climateVulnerability",         
    "Pl__C_B_S"="pollutionClimateBurden",                           
    "EnS_S"="finalScore",                        
    "Envrnmnt_2"="envExp_Pctl",                                      
    "Envrnmnt_3"="envEff_Pctl",                    
    "C_V_S_"="climate_Pctl",                        
    "S_P_S_"="senPop_Pctl" ,                                     
    "D_S_P"="socEco_Pctl",                          
    "P__C_B_S_P" ="pollClimBurden_Pctl",                
    "H__S_F_S_P" ="popCharacteristic_Pctl",                          
    "ES_S_" =  "finalScore_Pctl",
  )

# export as shps 
c2 <- county |>
  dplyr::left_join(y = renamed, by = "GEOID")

sf::st_write(c2,
             "data/products/enviroscreenScore/shp/countyEnviroScreenScores.shp",
             delete_dsn = TRUE)

ct2 <- censusTract |>
  dplyr::left_join(y = renamed, by = "GEOID")

sf::st_write(ct2, 
             "data/products/enviroscreenScore/shp/censusTractEnviroScreenScores.shp",
             delete_dsn = TRUE)


cbg2 <- cbg |>
  dplyr::left_join(y = renamed, by = "GEOID")

sf::st_write(cbg2,
             "data/products/enviroscreenScore/shp/censusBlockGroupEnviroScreenScores.shp",
             delete_dsn = TRUE)
