# BDA400 Assignment 5 - Simple Linear Regression
# Student: Samaneh Yarmohammadi

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || length(regressionSource) == 0) {
    stop("regressionSource must be a non-empty numeric vector")
  }
  if (regressionLength <= 1 || regressionLength != as.integer(regressionLength)) {
    stop("regressionLength must be an integer greater than 1")
  }
  if (regressionOffset < 0 || regressionOffset != as.integer(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }

  n <- length(regressionSource)
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)

  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  if (denominator == 0) {
    stop("linear regression requires at least two regression points")
  }

  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept

  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  return(result)
}
