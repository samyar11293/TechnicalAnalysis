# BDA400 Assignment 5 - Relative Strength Index
# Student: Samaneh Yarmohammadi

rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }
  if (period <= 0 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  if (length(data) <= period) {
    stop("data must contain more observations than period")
  }

  diff_values <- diff(data)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else if (diff_values[i] < 0) {
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))

  calculate_rsi <- function(gain, loss) {
    if (loss == 0 && gain == 0) return(50)
    if (loss == 0) return(100)
    if (gain == 0) return(0)
    rs <- gain / loss
    return(100 - (100 / (1 + rs)))
  }

  rsi_values[period + 1] <- calculate_rsi(avg_gain, avg_loss)
  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- calculate_rsi(avg_gain, avg_loss)
    }
  }
  return(rsi_values)
}
