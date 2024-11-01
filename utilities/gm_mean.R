#' Calculate Geometric Mean
#'
#' This function calculates the geometric mean of a numeric vector, handling potential zero values.
#' 
#' @param x A numeric vector.
#' 
#' @return The geometric mean of the non-zero values in x.
#'
#' @examples
#' x <- c(1, 2, 3, 0, 5)
#' gm_mean(x)  # Output: 2.310687

gm_mean <- function(x) {
  # Remove NA values from the vector (geometric mean is undefined for NA)
  x <- x[!is.na(x)]
  
  # If all values are NA, return NA
  if (length(x) == 0) {
    return(NA)
  }
  
  # Calculate geometric mean using the log and exponential trick (robust to zeros)
  geometric_mean <- exp(mean(log(x[x > 0])))
  
  return(geometric_mean)
}














#geometric mean
# take from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

gm_mean = function(x){
  # Drop the NA values from the list of features.
  # the reduced the length of the denominator
  x <- x[!is.na(x)]
  
  ### moving away from this due to potential diff between lenght in input in numerator/denomator when zeros are present
  # exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  
  exp(mean(log(x[x>0])))
  
}
