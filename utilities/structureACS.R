# helper function to transforming the long to wide data structure of ACS data
structureACS <- function(data){
  data |>
    tidyr::spread(key = variable, value = estimate) |>
    dplyr::group_by(GEOID) |>
    dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::group_by(GEOID)
}
