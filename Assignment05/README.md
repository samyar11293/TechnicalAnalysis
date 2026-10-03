# BDA400 Assignment 5 Technical Analysis Development Phase

**Student:** Samaneh Yarmohammadi  
**Course:** BDA400 Data Science Tools and Techniques  
**Repository:** <https://github.com/samyar11293/TechnicalAnalysis>

## Project contents

This folder contains independent base-R implementations of all nine required technical-analysis functions. No technical-analysis packages or existing indicator functions are used.

| File | Required function |
|---|---|
| `sma.R` | `sma(data, period)` |
| `ema.R` | `ema(data, period)` |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` |
| `stdev.R` | `stdev(data)` |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` |
| `rsi.R` | `rsi(data, period)` |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` |
| `crossover.R` | `crossover(arr1, arr2)` |
| `crossunder.R` | `crossunder(arr1, arr2)` |

## How to run

1. Download or clone the repository.
2. Open RStudio and set the working directory to `Assignment05`.
3. Run `source("run_tests.R")` for the complete verification suite.
4. For individual use, source the required file. Source `ema.R` before `macd.R`; source `sma.R` and `rsi.R` before `stoch_rsi.R`.

## Verification

`run_tests.R` checks numeric results, output lengths and structures, indicator ranges, and crossover/crossunder events. Each implementation also validates its arguments and returns clear error messages for invalid input.
