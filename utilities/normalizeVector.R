#' Normalize a Numeric Vector
#'
#' This function normalizes a numeric vector by dividing each element by the maximum value.
#'
#' @param x A numeric vector.
#'
#' @return A normalized numeric vector.

normalizeVector <- function(x) {
  # Calculate the maximum value, ignoring NA values
  max_value <- max(x, na.rm = TRUE)
  
  # Normalize the vector by dividing each element by the maximum
  normalized_vector <- x / max_value
  
  return(normalized_vector)
}