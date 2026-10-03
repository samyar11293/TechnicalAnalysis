# BDA400 Assignment 5 - Crossover Signals
# Student: Samaneh Yarmohammadi

crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) == 0) {
    stop("both arrays must be non-empty numeric vectors")
  }

  crossover_signals <- rep("None", length(arr1))
  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      }
    }
  }
  return(crossover_signals)
}
