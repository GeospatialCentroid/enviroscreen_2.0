#' Structure ACS Data
#'
#' This function transforms the long format of ACS data to a wide format and summarizes the specified variables.
#'
#' @param data A data frame in long format containing ACS data.
#'
#' @return A data frame in wide format with summarized variables.

structureACS <- function(data) {
  # Spread the data from long to wide format, grouping by GEOID
  data_wide <- data %>%
    tidyr::spread(key = variable, value = estimate) %>%
    dplyr::group_by(GEOID)
  
  # Summarize the specified variables (assuming they contain "_")
  data_summarized <- data_wide %>%
    dplyr::summarize(across(contains("_"), ~ sum(.x, na.rm = TRUE)))
  
  return(data_summarized)
}
