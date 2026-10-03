# BDA400 Assignment 5 - Crossunder Signals
# Student: Samaneh Yarmohammadi

crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) == 0) {
    stop("both arrays must be non-empty numeric vectors")
  }

  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"
  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      }
    }
  }
  return(crossunder_signals)
}
