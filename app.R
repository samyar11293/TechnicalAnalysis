# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Samaneh Yarmohammadi
# Purpose: Interactive R Shiny portfolio dashboard using Yahoo Finance data.
# Run from this folder with: shiny::runApp()

# =============================================================================
# STEP 1 - DATA COLLECTION AND SETUP
# =============================================================================

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(quantmod)
  library(TTR)
  library(scales)
})

# Converts an xts OHLC object to a consistently named data frame.
ohlc_to_data_frame <- function(x) {
  data.frame(
    Date = as.Date(index(x)),
    Open = as.numeric(Op(x)),
    High = as.numeric(Hi(x)),
    Low = as.numeric(Lo(x)),
    Close = as.numeric(Cl(x)),
    Volume = as.numeric(Vo(x)),
    Adjusted = if (has.Ad(x)) as.numeric(Ad(x)) else as.numeric(Cl(x)),
    check.names = FALSE
  )
}

# Aggregates daily observations into weekly or monthly OHLC bars.
aggregate_market_data <- function(x, time_frame) {
  switch(
    time_frame,
    "Daily" = x,
    "Weekly" = to.weekly(x, indexAt = "lastof", drop.time = TRUE),
    "Monthly" = to.monthly(x, indexAt = "lastof", drop.time = TRUE),
    x
  )
}

# =============================================================================
# STEP 2 - SHINY USER INTERFACE AND VISUALIZATION CONTROLS
# =============================================================================

dashboard_css <- "
body { background: #f3f6fa; color: #172033; }
.container-fluid { max-width: 1500px; }
.dashboard-title { font-weight: 700; letter-spacing: -0.02em; margin: 18px 0 2px; }
.dashboard-subtitle { color: #5d687a; margin-bottom: 20px; }
.well { background: #ffffff; border: 0; border-radius: 12px;
        box-shadow: 0 4px 18px rgba(30, 50, 80, 0.08); }
.kpi-row { display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px;
           margin: 0 0 16px; }
.kpi-card { background: #ffffff; border-radius: 10px; padding: 14px 16px;
            border-left: 4px solid #2f6fed;
            box-shadow: 0 3px 12px rgba(30, 50, 80, 0.07); }
.kpi-label { color: #657187; font-size: 12px; text-transform: uppercase;
             letter-spacing: 0.05em; }
.kpi-value { color: #16233a; font-size: 21px; font-weight: 700; margin-top: 4px; }
.section-card { background: #ffffff; border-radius: 12px; padding: 12px 16px;
                box-shadow: 0 4px 18px rgba(30, 50, 80, 0.08); margin-bottom: 16px; }
.help-note { color: #657187; font-size: 12px; line-height: 1.45; }
.btn-primary { background: #2f6fed; border-color: #2f6fed; width: 100%; }
@media (max-width: 900px) { .kpi-row { grid-template-columns: repeat(2, 1fr); } }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(dashboard_css))),
  div(class = "dashboard-title", h2("Portfolio Technical Analysis Dashboard")),
  div(
    class = "dashboard-subtitle",
    "Yahoo Finance market data with configurable charts, indicators, and trading signals"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("symbol", "Stock symbol", value = "AAPL", placeholder = "e.g., AAPL"),
      dateRangeInput(
        "date_range", "Date range",
        start = Sys.Date() - 365, end = Sys.Date(),
        min = as.Date("2000-01-01"), max = Sys.Date(), format = "yyyy-mm-dd"
      ),
      selectInput(
        "time_frame", "Time frame",
        choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"
      ),
      selectInput(
        "chart_type", "Price chart",
        choices = c("Line", "Candlestick", "Area"), selected = "Candlestick"
      ),
      checkboxGroupInput(
        "technical_indicators", "Technical indicators",
        choices = c(
          "Simple moving average" = "SMA",
          "Exponential moving average" = "EMA",
          "Relative strength index" = "RSI",
          "MACD" = "MACD"
        ),
        selected = c("SMA", "EMA", "RSI", "MACD")
      ),
      tags$hr(),
      h4("Indicator parameters"),
      fluidRow(
        column(6, numericInput("short_period", "Short MA", 20, min = 2, max = 200)),
        column(6, numericInput("long_period", "Long MA", 50, min = 3, max = 300))
      ),
      fluidRow(
        column(6, numericInput("rsi_period", "RSI period", 14, min = 2, max = 100)),
        column(6, numericInput("signal_period", "MACD signal", 9, min = 2, max = 50))
      ),
      checkboxInput("show_signals", "Show Buy/Sell annotations", value = TRUE),
      actionButton("refresh", "Load / Refresh Data", class = "btn-primary"),
      tags$hr(),
      downloadButton("download_data", "Download displayed data"),
      br(), br(),
      div(
        class = "help-note",
        "Signals occur only when the short SMA crosses the long SMA. Green = Buy; red = Sell. ",
        "The dashboard is for educational analysis and is not financial advice."
      )
    ),
    mainPanel(
      width = 9,
      uiOutput("status_message"),
      div(
        class = "kpi-row",
        div(class = "kpi-card", div(class = "kpi-label", "Latest close"), div(class = "kpi-value", textOutput("latest_close", inline = TRUE))),
        div(class = "kpi-card", div(class = "kpi-label", "Period return"), div(class = "kpi-value", textOutput("period_return", inline = TRUE))),
        div(class = "kpi-card", div(class = "kpi-label", "Latest signal"), div(class = "kpi-value", textOutput("latest_signal", inline = TRUE))),
        div(class = "kpi-card", div(class = "kpi-label", "Observations"), div(class = "kpi-value", textOutput("observation_count", inline = TRUE)))
      ),
      div(class = "section-card", plotOutput("stock_chart", height = "590px")),
      conditionalPanel(
        condition = "input.technical_indicators && (input.technical_indicators.indexOf('RSI') !== -1 || input.technical_indicators.indexOf('MACD') !== -1)",
        div(class = "section-card", plotOutput("oscillator_chart", height = "330px"))
      ),
      div(
        class = "section-card",
        h4("Most recent trading signals"),
        tableOutput("signal_table")
      )
    )
  )
)

# =============================================================================
# STEP 3 - DATA REACTIVITY AND TECHNICAL INDICATOR OVERLAYS
# =============================================================================

server <- function(input, output, session) {
  fetched_data <- eventReactive(input$refresh, {
    symbol <- toupper(trimws(input$symbol))
    validate(need(nchar(symbol) > 0, "Enter a stock symbol."))
    validate(need(input$date_range[1] < input$date_range[2], "Start date must be before end date."))

    withProgress(message = paste("Downloading", symbol), value = 0.25, {
      tryCatch(
        {
          raw <- suppressWarnings(getSymbols(
            Symbols = symbol,
            src = "yahoo",
            from = input$date_range[1],
            to = input$date_range[2] + 1,
            auto.assign = FALSE,
            warnings = FALSE
          ))
          incProgress(0.5, detail = "Preparing market data")
          validate(need(NROW(raw) > 1, "Yahoo Finance returned too few observations."))
          raw
        },
        error = function(e) {
          validate(need(FALSE, paste0(
            "Data download failed. Check the ticker, date range, and internet connection. Details: ",
            conditionMessage(e)
          )))
        }
      )
    })
  }, ignoreNULL = FALSE)

  chart_data <- reactive({
    x <- aggregate_market_data(fetched_data(), input$time_frame)
    df <- ohlc_to_data_frame(x)

    short_n <- as.integer(input$short_period)
    long_n <- as.integer(input$long_period)
    rsi_n <- as.integer(input$rsi_period)
    signal_n <- as.integer(input$signal_period)

    validate(need(short_n < long_n, "Short MA period must be smaller than long MA period."))
    validate(need(NROW(df) > long_n + 2, paste0(
      "Select a longer date range. At least ", long_n + 3,
      " aggregated observations are required."
    )))

    df$SMA_Short <- as.numeric(SMA(df$Close, n = short_n))
    df$SMA_Long <- as.numeric(SMA(df$Close, n = long_n))
    df$EMA <- as.numeric(EMA(df$Close, n = short_n))
    df$RSI <- as.numeric(RSI(df$Close, n = rsi_n))

    macd_values <- MACD(
      df$Close,
      nFast = short_n,
      nSlow = long_n,
      nSig = signal_n,
      maType = "EMA",
      percent = FALSE
    )
    df$MACD <- as.numeric(macd_values[, 1])
    df$MACD_Signal <- as.numeric(macd_values[, 2])
    df$MACD_Histogram <- df$MACD - df$MACD_Signal

    # A signal is emitted only on the exact crossover bar, not on every bar.
    above <- df$SMA_Short > df$SMA_Long
    previous_above <- c(NA, head(above, -1))
    df$Signal <- ifelse(
      !is.na(above) & !is.na(previous_above) & above & !previous_above,
      "Buy",
      ifelse(
        !is.na(above) & !is.na(previous_above) & !above & previous_above,
        "Sell",
        "Hold"
      )
    )
    df
  })

  base_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#172033"),
        plot.subtitle = element_text(color = "#657187"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title.x = element_blank()
      )
  }

  output$stock_chart <- renderPlot({
    df <- chart_data()
    bar_width <- switch(input$time_frame, "Daily" = 0.70, "Weekly" = 4.8, "Monthly" = 18, 0.70)
    title_text <- paste(toupper(trimws(input$symbol)), input$time_frame, "price")

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close, color = "Close"), linewidth = 0.75)
    } else if (input$chart_type == "Area") {
      p <- p +
        geom_area(aes(y = Close), fill = "#84a9f7", alpha = 0.38) +
        geom_line(aes(y = Close, color = "Close"), linewidth = 0.65)
    } else {
      candle_df <- transform(df, Direction = ifelse(Close >= Open, "Up", "Down"))
      p <- p +
        geom_segment(
          data = candle_df,
          aes(xend = Date, y = Low, yend = High, color = Direction),
          linewidth = 0.38
        ) +
        geom_rect(
          data = candle_df,
          aes(
            xmin = Date - bar_width / 2,
            xmax = Date + bar_width / 2,
            ymin = pmin(Open, Close), ymax = pmax(Open, Close),
            fill = Direction
          ),
          color = NA, alpha = 0.9
        ) +
        scale_fill_manual(values = c("Up" = "#1a9c68", "Down" = "#d64b4b")) +
        scale_color_manual(
          values = c(
            "Up" = "#1a9c68", "Down" = "#d64b4b", "Close" = "#2f6fed",
            "Short SMA" = "#f28e2b", "Long SMA" = "#7b61a8", "EMA" = "#00a6a6"
          )
        )
    }

    if ("SMA" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = SMA_Short, color = "Short SMA"), linewidth = 0.72, na.rm = TRUE) +
        geom_line(aes(y = SMA_Long, color = "Long SMA"), linewidth = 0.72, na.rm = TRUE)
    }
    if ("EMA" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = EMA, color = "EMA"), linewidth = 0.70, na.rm = TRUE)
    }

    # Ensure the legend works for line/area charts as well as candlesticks.
    if (input$chart_type != "Candlestick") {
      p <- p + scale_color_manual(values = c(
        "Close" = "#2f6fed", "Short SMA" = "#f28e2b",
        "Long SMA" = "#7b61a8", "EMA" = "#00a6a6"
      ))
    }

    if (isTRUE(input$show_signals)) {
      buy_points <- df[df$Signal == "Buy", , drop = FALSE]
      sell_points <- df[df$Signal == "Sell", , drop = FALSE]
      p <- p +
        geom_point(
          data = buy_points, aes(y = Low), inherit.aes = TRUE,
          shape = 24, size = 3.4, stroke = 0.9, fill = "#1a9c68", color = "#126c4a"
        ) +
        geom_text(
          data = buy_points, aes(y = Low, label = "BUY"),
          vjust = 1.8, color = "#126c4a", fontface = "bold", size = 3.2
        ) +
        geom_point(
          data = sell_points, aes(y = High), inherit.aes = TRUE,
          shape = 25, size = 3.4, stroke = 0.9, fill = "#d64b4b", color = "#992f2f"
        ) +
        geom_text(
          data = sell_points, aes(y = High, label = "SELL"),
          vjust = -0.9, color = "#992f2f", fontface = "bold", size = 3.2
        )
    }

    p +
      labs(
        title = title_text,
        subtitle = paste(
          format(min(df$Date), "%b %d, %Y"), "to",
          format(max(df$Date), "%b %d, %Y")
        ),
        y = "Price (USD)", color = NULL, fill = NULL
      ) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months", expand = expansion(mult = c(0.01, 0.03))) +
      guides(fill = guide_legend(order = 1), color = guide_legend(order = 2)) +
      coord_cartesian(clip = "off") +
      base_theme()
  }, res = 110)

  output$oscillator_chart <- renderPlot({
    df <- chart_data()
    selected <- input$technical_indicators
    validate(need(any(c("RSI", "MACD") %in% selected), "Select RSI or MACD."))

    if (all(c("RSI", "MACD") %in% selected)) {
      par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
      plot(df$Date, df$RSI, type = "l", col = "#2f6fed", lwd = 2,
           main = paste0("RSI (", input$rsi_period, ")"), xlab = "", ylab = "RSI", ylim = c(0, 100))
      abline(h = c(30, 70), col = c("#1a9c68", "#d64b4b"), lty = 2)
      bar_colors <- ifelse(df$MACD_Histogram >= 0, "#64b996", "#e18b8b")
      plot(df$Date, df$MACD_Histogram, type = "h", col = bar_colors, lwd = 3,
           main = "MACD", xlab = "Date", ylab = "Value")
      lines(df$Date, df$MACD, col = "#2f6fed", lwd = 2)
      lines(df$Date, df$MACD_Signal, col = "#f28e2b", lwd = 2)
      legend("topleft", c("MACD", "Signal"), col = c("#2f6fed", "#f28e2b"), lty = 1, bty = "n")
    } else if ("RSI" %in% selected) {
      plot(df$Date, df$RSI, type = "l", col = "#2f6fed", lwd = 2,
           main = paste0("Relative Strength Index (", input$rsi_period, ")"),
           xlab = "Date", ylab = "RSI", ylim = c(0, 100))
      abline(h = c(30, 70), col = c("#1a9c68", "#d64b4b"), lty = 2)
    } else {
      bar_colors <- ifelse(df$MACD_Histogram >= 0, "#64b996", "#e18b8b")
      plot(df$Date, df$MACD_Histogram, type = "h", col = bar_colors, lwd = 3,
           main = "Moving Average Convergence Divergence", xlab = "Date", ylab = "Value")
      lines(df$Date, df$MACD, col = "#2f6fed", lwd = 2)
      lines(df$Date, df$MACD_Signal, col = "#f28e2b", lwd = 2)
      legend("topleft", c("MACD", "Signal"), col = c("#2f6fed", "#f28e2b"), lty = 1, bty = "n")
    }
  }, res = 110)

  # =============================================================================
  # STEP 4 - TRADING SIGNAL OUTPUTS, ANNOTATIONS, AND DOWNLOAD
  # =============================================================================

  output$latest_close <- renderText({
    df <- chart_data()
    dollar(tail(df$Close, 1), accuracy = 0.01)
  })

  output$period_return <- renderText({
    df <- chart_data()
    return_value <- tail(df$Close, 1) / head(df$Close, 1) - 1
    percent(return_value, accuracy = 0.01)
  })

  output$latest_signal <- renderText({
    df <- chart_data()
    non_hold <- df[df$Signal != "Hold", , drop = FALSE]
    if (NROW(non_hold) == 0) "No crossover" else paste(tail(non_hold$Signal, 1), format(tail(non_hold$Date, 1), "%b %d"))
  })

  output$observation_count <- renderText({
    comma(NROW(chart_data()))
  })

  output$signal_table <- renderTable({
    df <- chart_data()
    signals_only <- df[df$Signal != "Hold", c("Date", "Close", "SMA_Short", "SMA_Long", "Signal")]
    if (NROW(signals_only) == 0) {
      return(data.frame(Message = "No moving-average crossover occurred in the selected period."))
    }
    signals_only <- tail(signals_only, 10)
    signals_only$Close <- dollar(signals_only$Close, accuracy = 0.01)
    signals_only$SMA_Short <- dollar(signals_only$SMA_Short, accuracy = 0.01)
    signals_only$SMA_Long <- dollar(signals_only$SMA_Long, accuracy = 0.01)
    names(signals_only) <- c("Date", "Close", "Short SMA", "Long SMA", "Signal")
    signals_only
  }, striped = TRUE, bordered = FALSE, spacing = "s", align = "l")

  output$status_message <- renderUI({
    df <- chart_data()
    tags$div(
      class = "alert alert-success",
      paste(
        "Loaded", NROW(df), tolower(input$time_frame), "observations for",
        toupper(trimws(input$symbol)), "from Yahoo Finance."
      )
    )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0(toupper(trimws(input$symbol)), "_", tolower(input$time_frame), "_analysis.csv")
    },
    content = function(file) {
      write.csv(chart_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
