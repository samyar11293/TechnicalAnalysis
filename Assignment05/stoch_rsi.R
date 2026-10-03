# BDA400 Assignment 5 - Stochastic RSI
# Student: Samaneh Yarmohammadi
# Dependencies: source("sma.R") and source("rsi.R") first.

stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function") || !exists("sma", mode = "function")) {
    stop("rsi() and sma() are required. Source rsi.R and sma.R first")
  }
  periods <- c(period, k_period, d_period)
  if (any(is.na(periods)) || any(periods <= 0) || any(periods != as.integer(periods))) {
    stop("all periods must be positive integers")
  }

  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) < k_period) {
    stop("not enough valid RSI values for k_period")
  }

  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  }

  k_line <- sma(k_values, k_period)
  if (length(k_line) < d_period) {
    stop("not enough %K values for d_period")
  }
  d_line <- sma(k_line, d_period)

  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
