# helper function to transforming the long to wide data structure of ACS data
structureACS <- function(data){
  data |>
    tidyr::spread(key = variable, value = estimate) |>
    dplyr::group_by(GEOID) |>
    dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE))) 
    # adding this second group by to the specific indicator functions 
    # |>
    #   dplyr::group_by(GEOID)
}
