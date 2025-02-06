###
# develop the MOE estimates for all ACS data 
# 
#
### 


# want to rely on tidycensus calculations for this work 
# options 
?tidycensus::moe_sum # A sum is the result of adding two or more numbers together.
?tidycensus::moe_product # A product is the result of multiplying two or more numbers together.
?tidycensus::moe_ratio # A ratio compares two quantities. It shows how many times one quantity contains another.
?tidycensus::moe_prop # A proportion is a specific type of ratio where the denominator represents the total quantity, and the numerator represents a part of that total

# for enviroscreen measures we likely only care about moe_sum and moe_product 
# as an example the percent population under 5 measures needs both estimates 
# (male pop under 5 + female pop under 5)/total pop 
# we need to moe_sum estimate of all values in the numerator 
# then determine the moe_prop estimate of that calculated value against the whole
# this is done in a rowwise calculation so that all numbers are unique to a census block group
# For accounting, the enviroscreen score value for the given indicator is also calculated and provided
# this is not ment to be a replacement for the enviroscreen score in any way 

propMOE_estimation <- function(columns_vect, totalPop_df){
  # generate vectors of estimates and moes 
  measures <- paste0(columns_vect, "E")
  moe <- paste0(columns_vect, "M")
  
  # pull acs values 
  acs <- get_acs(geography = "cbg",
                 variables = columns_vect,
                 state = 08,
                 year = 2020, # Or any desired year
                 survey = "acs5", # 1-year or 5-year ACS data
                 output = "wide") 
  # join to population data
  # testing some tidyverse methods 
  acs <- acs |>
    dplyr::left_join(y = totalPop_df, by = "GEOID")|>
    dplyr::rowwise()|>
    dplyr::mutate(
      # total count
      num_est = sum(across(all_of(measures)),na.rm = TRUE),
      # combined MOE estimate 
      num_moe = moe_sum(estimate = across(all_of(measures)),
                        moe = across(all_of(moe))),
      # enviroscreen value 
      enviroscreenValue = (num_est/total_pop)*100,
      # proportion estimate 
      prop_moe = moe_prop(
        num = num_est, 
        moe_num = num_moe,
        denom = total_pop ,
        moe_denom = total_pop_moe 
      )
    )
}

# reference layer 
allVar <- tidycensus::load_variables(year = "2020", dataset = "acs5")

# indicators needed 
totalPop = c("B01003_001")
under5 = c("B01001_003","B01001_027") 
over64 = c( paste0("B01001_0", 20:25),paste0("B01001_0", 44:49))
poc = c("B03002_001","B03002_003")
lowIncome = c("C17002_001","C17002_008")
lingu = c("C16002_001","C16002_004","C16002_007","C16002_010","C16002_013")
highSchool = c("B15002_001", paste0("B15002_00", 2:9),"B15002_010",paste0("B15002_0", 20:27))
disability = c(paste0("B18101_", c("001","004","007","010","013","016","019","023",
                      "026","029","032","035","038")))
lead = c("B25034_001", "B25034_009","B25034_010", "B25034_011")
housingBurden = c("B25070_001", "B25070_007", "B25070_008", "B25070_009", "B25070_010", 
  "B25091_001", "B25091_008", "B25091_009", "B25091_010", "B25091_011",  "B25091_019", 
  "B25091_020", "B25091_021", "B25091_022" )

# total pop 
t_pop <- get_acs(geography = "cbg",
                 variables = totalPop,
                 state = "08",
                 year = 2020, # Or any desired year
                 survey = "acs5", # 1-year or 5-year ACS data
                 output = "wide")|>
  dplyr::rename(total_pop = B01003_001E,
                total_pop_moe = B01003_001M)|>
  dplyr::select(GEOID,total_pop,total_pop_moe)

# under 5 -----------------------------------------------------------------
u5 <- propMOE_estimation(columns_vect = under5, 
                         totalPop_df = t_pop)
# export 
readr::write_csv(x = u_5a,file = "data/products/acsMOE_estimates/ageUnder5.csv")

# over 65 -----------------------------------------------------------------
o64 <- propMOE_estimation(columns_vect = over64, 
                         totalPop_df = t_pop)
# export 
readr::write_csv(x = u_5a,file = "data/products/acsMOE_estimates/over64.csv")

# poeple of color -----------------------------------------------------------------
## reference for calculation 
# percent_minority = ifelse(B03002_001 == 0,
#                           NA,
#                           (B03002_001 - B03002_003) /
#                             B03002_001))
# because of this subtraction we need to calculate a bit different 
# B03002_001 : total people -- technicallly the same measures from t_pop but using for consistency with original work
# B03002_003 : Estimate!!Total:!!Not Hispanic or Latino:!!White alone
# so rather then subtract 003, we will add all other measures 
poc <- c("B03002_002", paste0("B03002_00", 4:9))
# generate vectors of estimates and moes 
measures <- paste0(poc, "E")
moe <- paste0(poc, "M")

# pull acs values 
acs <- get_acs(geography = "cbg",
               variables = c("B03002_001", poc),
               state = 08,
               year = 2020, # Or any desired year
               survey = "acs5", # 1-year or 5-year ACS data
               output = "wide") 
# join to population data
# testing some tidyverse methods 
poc1 <- acs |>
  # dplyr::left_join(y = totalPop_df, by = "GEOID")|>
  dplyr::rowwise()|>
  dplyr::mutate(
    # total count
    num_est = sum(across(all_of(measures)),na.rm = TRUE),
    # combined MOE estimate 
    num_moe = moe_sum(estimate = across(all_of(measures)),
                      moe = across(all_of(moe))),
    # enviroscreen value 
    enviroscreenValue = (num_est/B03002_001E)*100,
    # proportion estimate 
    prop_moe = moe_prop(
      num = num_est, 
      moe_num = num_moe,
      denom = B03002_001E ,
      moe_denom = B03002_001M 
    )
  )


# export 
readr::write_csv(x = poc1,file = "data/products/acsMOE_estimates/poc.csv")


# low income -----------------------------------------------------------------
## reference calc
# percent_lowincome = ifelse(C17002_001 == 0, NA, (C17002_001 - C17002_008) / C17002_001))|>
# because of this subtraction we need to calculate a bit different 
# C17002_001 : RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
# C17002_008 : Estimate!!Total:!!2.00 and over
# so rather then subtract C17002_008, we will add all other measures 
lowIncome <- paste0("C17002_00", 2:7)
# generate vectors of estimates and moes 
measures <- paste0(lowIncome, "E")
moe <- paste0(lowIncome, "M")

# pull acs values 
acs <- get_acs(geography = "cbg",
               variables = c("C17002_001", lowIncome),
               state = 08,
               year = 2020, # Or any desired year
               survey = "acs5", # 1-year or 5-year ACS data
               output = "wide") 
# join to population data
# testing some tidyverse methods 
low1 <- acs |>
  # dplyr::left_join(y = totalPop_df, by = "GEOID")|>
  dplyr::rowwise()|>
  dplyr::mutate(
    # total count
    num_est = sum(across(all_of(measures)),na.rm = TRUE),
    # combined MOE estimate 
    num_moe = moe_sum(estimate = across(all_of(measures)),
                      moe = across(all_of(moe))),
    # enviroscreen value 
    enviroscreenValue = (num_est/C17002_001E)*100,
    # proportion estimate 
    prop_moe = moe_prop(
      num = num_est, 
      moe_num = num_moe,
      denom = C17002_001E ,
      moe_denom = C17002_001M 
    )
  )


# export 
readr::write_csv(x = low1,file = "data/products/acsMOE_estimates/lowIncome.csv")
