# BDA400 Assignment 5 - Reproducible verification tests

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

check <- function(condition, label) {
  if (!isTRUE(condition)) stop(paste("FAILED:", label))
  cat("PASS:", label, "\n")
}

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
check(isTRUE(all.equal(sma(data, 3), c(12.333333, 15.666667, 17.666667, 20, 21.666667, 23.666667, 23.333333), tolerance = 1e-6)), "SMA")
check(length(ema(data, 3)) == length(data), "EMA length")

m <- macd(data, 3, 5, 2)
check(all(c("macd_line", "signal_line", "histogram") %in% names(m)), "MACD structure")
check(length(m$macd_line) == length(data), "MACD length")

expected_population_sd <- sqrt(sum((data - sum(data) / length(data))^2) / length(data))
check(isTRUE(all.equal(stdev(data), expected_population_sd)), "Population standard deviation")

lr <- linreg(c(2, 4, 6, 8, 10), 5, 0)
check(isTRUE(all.equal(lr$slope, 2)), "Linear regression slope")
check(isTRUE(all.equal(lr$intercept, 0)), "Linear regression intercept")

price <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 66, 68, 64, 70, 72, 69, 75, 78, 74, 80, 82, 79, 85, 88, 84, 90, 92, 89, 95, 97)
r <- rsi(price, 5)
check(length(r) == length(price) && all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100), "RSI range and length")

sr <- stoch_rsi(price, 5, 3, 3)
check(all(sr$k_line >= 0 & sr$k_line <= 1), "StochRSI %K range")
check(all(sr$d_line >= 0 & sr$d_line <= 1), "StochRSI %D range")

a <- c(1, 3, 1, 4)
b <- c(2, 2, 2, 2)
check(identical(crossover(a, b), c("None", "Up", "Down", "Up")), "Crossover")
check(identical(crossunder(a, b), c("None", "False", "True", "False")), "Crossunder")

cat("\nAll Assignment 5 verification tests passed.\n")
