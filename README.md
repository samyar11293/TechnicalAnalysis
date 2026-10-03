# BDA400 Assignment 6 - Portfolio Technical Analysis Dashboard

**Student:** Samaneh Yarmohammadi  
**Course:** BDA400 - Data Science Tools and Techniques  
**Assignment:** A06 - Technical Analysis using R, Visualization Phase

## Project description

This R Shiny application downloads historical stock data from Yahoo Finance and presents an interactive technical-analysis dashboard. Users can change the ticker, date range, time frame, and price-chart style. Simple and exponential moving averages can be overlaid on the price chart, while RSI and MACD appear in a synchronized indicator panel. A configurable short/long SMA crossover rule produces Buy and Sell annotations only on the bars where a crossover occurs.

## Repository link

<https://github.com/samyar11293/TechnicalAnalysis>

## Features mapped to the assignment

1. **Data collection and setup** - Yahoo Finance data through `quantmod::getSymbols()` with ticker/date validation and readable error handling.
2. **Stock visualization** - Daily, weekly, or monthly data shown as line, area, or candlestick charts.
3. **Technical indicators** - User-controlled SMA, EMA, RSI, and MACD with adjustable periods.
4. **Trading rules and annotations** - Exact short/long SMA crossovers generate Buy and Sell points, labels, and a recent-signal table.
5. **Usability** - KPI cards, responsive layout, CSV download, progress feedback, and an educational-use disclaimer.

## Requirements

- R 4.2 or later
- Internet access (Yahoo Finance data are downloaded at runtime)
- Packages: `shiny`, `ggplot2`, `quantmod`, `TTR`, and `scales`

The script installs a missing package automatically from CRAN. Packages can also be installed manually:

```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "scales"))
```

## How to run

1. Download or clone the repository.
2. Open RStudio.
3. Set the working directory to `TechnicalAnalysis/Assignment06`.
4. Run:

```r
shiny::runApp()
```

5. Enter a valid Yahoo Finance symbol, choose settings, and click **Load / Refresh Data**.

## Trading-rule definition

- **Buy:** the short SMA is above the long SMA on the current bar and was not above it on the previous bar.
- **Sell:** the short SMA is below or equal to the long SMA on the current bar and was above it on the previous bar.
- **Hold:** no crossover occurs. Hold values remain in the downloaded analysis but are not cluttered onto every chart bar.

## Important implementation note

SMA and EMA share the price scale and are overlaid directly on the price chart. RSI and MACD use different units, so they are displayed in a synchronized oscillator panel rather than being misleadingly forced onto the price axis.

## Verification checklist

- [ ] Run `parse(file = "app.R")` with no syntax error.
- [ ] Run `shiny::runApp()` and confirm the AAPL default view loads.
- [ ] Test Daily, Weekly, and Monthly time frames.
- [ ] Test Line, Candlestick, and Area charts.
- [ ] Turn each indicator on and off independently.
- [ ] Confirm short period is less than long period.
- [ ] Confirm invalid symbols and short date ranges show friendly errors.
- [ ] Confirm Buy/Sell annotations and the signal table agree.
- [ ] Verify the public repository link in a private browser window.

## Academic and financial-use notice

This dashboard was prepared for a course assignment. Its trading signals are simplified educational examples, not personalized investment advice.
