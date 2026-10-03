# BDA400 Assignment 5 - Population Standard Deviation
# Student: Samaneh Yarmohammadi

stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }

  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values^2
  variance <- sum(squared_diff) / length(data)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
