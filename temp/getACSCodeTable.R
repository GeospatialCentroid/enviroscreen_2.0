# might has this as a independent call or just leave as a temp file not sure yet.... 


## ACS data variables 
acsVars <- tidycensus::load_variables(year = 2022,dataset = "acs5")
write_csv(acsVars, file = "data/products/acsLabels.csv")
