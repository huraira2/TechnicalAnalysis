# =============================================================================
# app.R - Portfolio Dashboard (R Shiny)
# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Nadeeka Godage (664430944)
#
# Third stage of the project:
#   * Assignment 2 - portfolio (AAPL, MSFT, GOOGL, AMZN, TSLA) and data loading
#   * Assignment 5 - base-R indicators: sma, ema, macd, rsi, stdev, linreg,
#                    crossover, crossunder (re-used here, see Step 3)
#   * Assignment 6 - this interactive dashboard
#
# HOW TO RUN
#   install.packages(c("shiny", "bslib", "ggplot2", "quantmod", "patchwork", "scales"))
#   Then either open this file in RStudio and click "Run App", or run from the
#   repository folder:   shiny::runApp("Assignment6")
#
# The code is organised in the same sections as the assignment instructions:
#   STEP 1  Data collection and setup
#   STEP 2  Visualizing stock data (Shiny skeleton, widgets, chart types)
#   STEP 3  Overlay technical indicators (each one can be turned on and off)
#   STEP 4  Trading rules and annotations
#   UI and SERVER
#
# AI declaration: I used Claude (Anthropic, Opus 5.5, November 2026) to help
# write and debug this app. I tested every feature and I am responsible for
# the final work.
# =============================================================================


# =============================================================================
# STEP 1 - DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1a. Install and load the necessary packages ----------------------------
# Stop with a clear message (instead of a cryptic error) if a package is missing.
required_packages <- c("shiny", "bslib", "ggplot2", "quantmod", "patchwork", "scales")
missing_packages  <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Please install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_packages, '"', collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(shiny)      # web app framework
  library(bslib)      # modern Bootstrap theme, cards and value boxes
  library(ggplot2)    # charts
  library(quantmod)   # fetching stock data from Yahoo Finance, weekly/monthly bars
  library(patchwork)  # stacking the price, volume, RSI and MACD panels
})

# ---- 1b. Re-use the indicators written in Assignment 5 (base R only) --------
# The app is run from the Assignment6 folder, so the previous part is one level up.
indicator_dir <- if (dir.exists("../Assignment5")) "../Assignment5" else "Assignment5"
for (f in c("sma.R", "ema.R", "macd.R", "rsi.R", "stdev.R", "linreg.R",
            "crossover.R", "crossunder.R")) {
  source(file.path(indicator_dir, f), local = TRUE)
}

# ---- 1c. Choose a data source ------------------------------------------------
# Main source: Yahoo Finance (live, through quantmod::getSymbols).
# Back-up source: the Yahoo prices saved as CSV in Assignment 5. It is used
# automatically if Yahoo cannot be reached, so the app still works offline.
offline_csv <- file.path(indicator_dir, "stock_prices.csv")

# Portfolio from Assignment 2 (falls back to the same five symbols)
portfolio_file <- "../Assignment2/portfolio.txt"
portfolio <- if (file.exists(portfolio_file)) {
  toupper(trimws(scan(portfolio_file, what = character(), quiet = TRUE)))
} else {
  c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
}

# Same fixed period as Assignment 2 and 5, so results can be compared
default_start <- as.Date("2025-08-24")
default_end   <- as.Date("2026-08-24")

# Extra history fetched before the chosen start date so that long indicators
# (e.g. a 50-day moving average) already have values on the first day shown.
history_buffer_days <- 4 * 365

# ---- 1d. Fetch historical stock data (with error handling) -------------------
# Results are cached in memory so changing a widget does not download again.
data_cache <- new.env()

# Make sure the data has clean Open/High/Low/Close/Volume columns and no gaps.
clean_ohlc <- function(x) {
  x <- x[, 1:5]
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
  missing_before <- sum(is.na(x))
  x <- na.locf(x, na.rm = FALSE)           # fill a missing price with the day before
  x <- x[!is.na(x$Close), ]                # drop rows that still have no price
  x$Volume[is.na(x$Volume)] <- 0
  attr(x, "missing_fixed") <- missing_before
  x
}

read_offline <- function(symbol, from, to) {
  if (!file.exists(offline_csv)) return(NULL)
  df <- read.csv(offline_csv, stringsAsFactors = FALSE)
  df <- df[df$Symbol == symbol, ]
  if (nrow(df) == 0) return(NULL)
  df$Date <- as.Date(df$Date)
  df <- df[df$Date >= from & df$Date <= to, ]
  if (nrow(df) == 0) return(NULL)
  xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
}

fetch_stock <- function(symbol, from, to, source = "Yahoo Finance") {
  symbol <- toupper(trimws(symbol))
  key <- paste(symbol, from, to, source, sep = "|")
  if (!is.null(data_cache[[key]])) return(data_cache[[key]])

  result <- list(data = NULL, source = NA, message = NULL)

  # Try Yahoo Finance first (unless the offline source was chosen)
  if (source == "Yahoo Finance") {
    stock_data <- tryCatch(
      suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to + 1,
                                  auto.assign = FALSE)),
      error = function(e) {
        result$message <<- paste("Yahoo Finance failed:", conditionMessage(e))
        NULL
      })
    if (!is.null(stock_data) && nrow(stock_data) > 0) {
      result$data   <- clean_ohlc(stock_data)
      result$source <- "Yahoo Finance (live)"
    }
  }

  # Fall back to the saved CSV
  if (is.null(result$data)) {
    stock_data <- read_offline(symbol, from, to)
    if (!is.null(stock_data)) {
      result$data   <- clean_ohlc(stock_data)
      result$source <- "Offline CSV (Yahoo prices saved in Assignment 5)"
    } else if (is.null(result$message)) {
      result$message <- paste0("No offline data for ", symbol,
                               ". The offline file only has ", paste(portfolio, collapse = ", "),
                               " from 2025-08-25 to 2026-08-21.")
    } else {
      result$message <- paste(result$message, "- and no offline data for", symbol)
    }
  }

  if (!is.null(result$data)) assign(key, result, envir = data_cache)
  result
}


# =============================================================================
# STEP 2 - VISUALIZING STOCK DATA (helpers used by the server)
# =============================================================================

# Convert daily bars to weekly or monthly bars (time frame widget)
apply_time_frame <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)
  period <- if (time_frame == "Weekly") "weeks" else "months"
  out <- to.period(x, period = period, indexAt = "endof", OHLC = TRUE, name = NULL)
  colnames(out) <- c("Open", "High", "Low", "Close", "Volume")
  out
}

up_colour   <- "#16a34a"
down_colour <- "#dc2626"

# Draw the price using the chosen chart type
price_layers <- function(d, chart_type) {
  half_width <- 0.35 * as.numeric(median(diff(d$Date)))
  d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
  switch(chart_type,
    "Line" = list(geom_line(aes(y = Close), colour = "#1f2937", linewidth = 0.6)),
    "Area" = list(
      geom_ribbon(aes(ymin = min(d$Low, na.rm = TRUE), ymax = Close), fill = "#93c5fd", alpha = 0.45),
      geom_line(aes(y = Close), colour = "#1d4ed8", linewidth = 0.6)),
    "Candlestick" = list(
      geom_segment(data = d, aes(xend = Date, y = Low, yend = High, colour = Direction),
                   linewidth = 0.35, show.legend = FALSE),
      geom_rect(data = d, aes(xmin = Date - half_width, xmax = Date + half_width,
                              ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Direction),
                colour = NA, show.legend = FALSE)),
    "OHLC Bars" = list(
      geom_segment(data = d, aes(xend = Date, y = Low, yend = High, colour = Direction),
                   linewidth = 0.4, show.legend = FALSE),
      geom_segment(data = d, aes(x = Date - half_width, xend = Date, y = Open, yend = Open,
                                 colour = Direction), linewidth = 0.4, show.legend = FALSE),
      geom_segment(data = d, aes(x = Date, xend = Date + half_width, y = Close, yend = Close,
                                 colour = Direction), linewidth = 0.4, show.legend = FALSE))
  )
}


# =============================================================================
# STEP 3 - TECHNICAL INDICATORS (Assignment 5 functions, plus small helpers)
# =============================================================================

# sma() returns only complete windows; pad the front with NA so it lines up
pad_front <- function(values, n) c(rep(NA_real_, n - length(values)), values)

moving_average <- function(x, n, type) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  if (type == "SMA") pad_front(sma(x, n), length(x)) else ema(x, n)
}

# Rolling standard deviation built from the Assignment 5 stdev() (for Bollinger Bands)
rolling_stdev <- function(x, n) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= n) for (i in n:length(x)) out[i] <- stdev(x[(i - n + 1):i])
  out
}

# Add every indicator column to the data (calculated on the full history,
# so values are correct from the first day of the selected range)
add_indicators <- function(x, p) {
  d <- data.frame(Date = as.Date(index(x)), coredata(x))
  close <- d$Close
  d$MA_short <- moving_average(close, p$ma_short, p$ma_type)
  d$MA_long  <- moving_average(close, p$ma_long,  p$ma_type)
  d$BB_mid   <- moving_average(close, p$bb_n, "SMA")
  bb_sd      <- rolling_stdev(close, p$bb_n)
  d$BB_upper <- d$BB_mid + p$bb_k * bb_sd
  d$BB_lower <- d$BB_mid - p$bb_k * bb_sd
  d$RSI      <- if (length(close) > p$rsi_n) suppressWarnings(rsi(close, p$rsi_n)) else NA_real_
  m <- macd(close, p$macd_fast, p$macd_slow, p$macd_signal)
  d$MACD        <- m$macd_line
  d$MACD_signal <- m$signal_line
  d$MACD_hist   <- m$histogram
  d
}


# =============================================================================
# STEP 4 - TRADING RULES AND ANNOTATIONS
# =============================================================================
# Two outputs per bar:
#   Signal - the state rule from the brief: Buy while the rule is bullish,
#            Sell while it is bearish, otherwise Hold.
#   Action - the bar where the signal changes (the actual trade): Buy / Sell /
#            Hold. These are the bars annotated on the chart.

trading_rules <- c(
  "Moving Average Crossover"     = "ma",
  "RSI Overbought / Oversold"    = "rsi",
  "MACD Signal-line Crossover"   = "macd",
  "MA Crossover + RSI filter"    = "ma_rsi"
)

generate_signals <- function(d, rule, rsi_low, rsi_high) {
  n <- nrow(d)
  action <- rep("Hold", n)
  signal <- rep("Hold", n)
  ma_cross   <- crossover(d$MA_short, d$MA_long)          # "Up" / "Down" / "None"
  macd_cross <- crossover(d$MACD, d$MACD_signal)
  rsi_up     <- crossover(d$RSI, rep(rsi_low, n)) == "Up"      # RSI rises back above oversold
  rsi_down   <- crossunder(d$RSI, rep(rsi_high, n)) == "True"  # RSI falls back below overbought

  if (rule == "ma") {
    # Brief's rule: short MA above long MA -> Buy, below -> Sell, otherwise Hold
    signal <- ifelse(d$MA_short > d$MA_long, "Buy", ifelse(d$MA_short < d$MA_long, "Sell", "Hold"))
    action[ma_cross == "Up"]   <- "Buy"
    action[ma_cross == "Down"] <- "Sell"
  } else if (rule == "rsi") {
    signal <- ifelse(d$RSI < rsi_low, "Buy", ifelse(d$RSI > rsi_high, "Sell", "Hold"))
    action[rsi_up]   <- "Buy"
    action[rsi_down] <- "Sell"
  } else if (rule == "macd") {
    signal <- ifelse(d$MACD > d$MACD_signal, "Buy", ifelse(d$MACD < d$MACD_signal, "Sell", "Hold"))
    action[macd_cross == "Up"]   <- "Buy"
    action[macd_cross == "Down"] <- "Sell"
  } else if (rule == "ma_rsi") {
    # Buy on an MA up-cross only if the stock is not already overbought;
    # Sell on an MA down-cross, or when RSI falls back from overbought.
    ok_rsi <- !is.na(d$RSI) & d$RSI < rsi_high
    signal <- ifelse(d$MA_short > d$MA_long & ok_rsi, "Buy",
                     ifelse(d$MA_short < d$MA_long, "Sell", "Hold"))
    action[ma_cross == "Up" & ok_rsi]        <- "Buy"
    action[ma_cross == "Down" | rsi_down]    <- "Sell"
  }
  signal[is.na(signal)] <- "Hold"
  d$Signal <- signal
  d$Action <- action
  d
}

# Simple back-test: buy on a Buy action, sell on a Sell action, trade at the
# close of that bar. Compared with simply holding the stock for the whole range.
backtest <- function(d) {
  n <- nrow(d)
  position <- numeric(n)
  holding <- 0
  for (i in seq_len(n)) {
    if (d$Action[i] == "Buy")  holding <- 1
    if (d$Action[i] == "Sell") holding <- 0
    position[i] <- holding
  }
  bar_return <- c(0, d$Close[-1] / d$Close[-n] - 1)
  strategy   <- c(0, position[-n]) * bar_return     # yesterday's position earns today's move
  list(strategy  = prod(1 + strategy) - 1,
       buy_hold  = d$Close[n] / d$Close[1] - 1,
       trades    = sum(d$Action != "Hold"),
       in_market = mean(position))
}

# Shaded background for Buy / Sell zones (runs of the same Signal)
signal_zones <- function(d) {
  r <- rle(d$Signal)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  z <- data.frame(start = d$Date[starts], end = d$Date[ends], Signal = r$values)
  z[z$Signal != "Hold", ]
}

pct <- function(x) sprintf("%+.1f%%", 100 * x)


# =============================================================================
# UI - layout and interactive widgets
# =============================================================================

ui <- page_sidebar(
  title = "Portfolio Technical Analysis Dashboard - Nadeeka Godage (BDA400)",
  theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_collection("Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")),
  fillable = FALSE,

  sidebar = sidebar(
    width = 320,
    # ---- Step 1 / 2 widgets: stock, source, dates, time frame, chart type ----
    selectizeInput("symbol", "Stock symbol (type any Yahoo ticker):",
                   choices = portfolio, selected = portfolio[1],
                   options = list(create = TRUE)),
    selectInput("data_source", "Data source:",
                choices = c("Yahoo Finance", "Offline CSV")),
    dateRangeInput("date_range", "Select Date Range:",
                   start = default_start, end = default_end, max = Sys.Date()),
    selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
    radioButtons("chart_type", "Chart type:", inline = TRUE,
                 choices = c("Candlestick", "Line", "Area", "OHLC Bars")),

    # ---- Step 3 widgets: turn each indicator on or off ----
    checkboxGroupInput("technical_indicators", "Technical indicators (on / off):",
                       choices = c("Moving Averages", "Bollinger Bands", "Trend Line",
                                   "Volume", "RSI", "MACD"),
                       selected = c("Moving Averages", "RSI", "MACD")),

    # ---- Step 4 widgets: trading rule and annotations ----
    selectInput("rule", "Trading rule:", choices = trading_rules),
    checkboxInput("show_signals", "Annotate Buy / Sell bars", TRUE),
    checkboxInput("show_zones", "Shade Buy / Sell zones", FALSE),
    checkboxInput("label_all", "Label every bar with its signal (Buy / Sell / Hold)", FALSE),

    accordion(
      open = FALSE,
      accordion_panel("Moving average settings",
        radioButtons("ma_type", NULL, choices = c("SMA", "EMA"), inline = TRUE),
        sliderInput("ma_short", "Short-term period", 5, 100, 20),
        sliderInput("ma_long", "Long-term period", 10, 250, 50)),
      accordion_panel("Bollinger / trend settings",
        sliderInput("bb_n", "Bollinger period", 5, 100, 20),
        sliderInput("bb_k", "Band width (standard deviations)", 1, 3, 2, step = 0.5),
        sliderInput("trend_n", "Trend line: last N bars (linreg)", 10, 250, 60)),
      accordion_panel("RSI settings",
        sliderInput("rsi_n", "RSI period", 2, 50, 14),
        sliderInput("rsi_levels", "Oversold / overbought levels", 0, 100, c(30, 70))),
      accordion_panel("MACD settings",
        sliderInput("macd_fast", "Fast EMA", 2, 50, 12),
        sliderInput("macd_slow", "Slow EMA", 5, 100, 26),
        sliderInput("macd_signal", "Signal EMA", 2, 50, 9))
    )
  ),

  # ---- Summary boxes ----
  layout_column_wrap(
    width = 1 / 4, fill = FALSE,
    value_box(title = "Last close", value = textOutput("vb_close"),
              p(textOutput("vb_change", inline = TRUE), " over the range"),
              theme = "primary"),
    value_box(title = "Current signal", value = textOutput("vb_signal"),
              p(textOutput("vb_rule", inline = TRUE)), theme = "secondary"),
    value_box(title = "Strategy return", value = textOutput("vb_strategy"),
              p(textOutput("vb_trades", inline = TRUE)), theme = "success"),
    value_box(title = "Buy & hold return", value = textOutput("vb_buyhold"),
              p("Holding the stock for the whole range"), theme = "info")
  ),

  navset_card_tab(
    nav_panel("Chart",
      textOutput("source_note"),
      plotOutput("stock_chart", height = "auto")),
    nav_panel("Signals",
      p("Bars where the trading rule gives a trade (Buy or Sell), newest first."),
      tableOutput("signal_table")),
    nav_panel("Portfolio",
      p("All portfolio stocks over the selected date range and time frame, using the same rule and settings."),
      tableOutput("portfolio_table"),
      plotOutput("portfolio_chart", height = "380px")),
    nav_panel("Data",
      verbatimTextOutput("data_info"),
      tableOutput("data_tail")),
    nav_panel("About",
      h5("How it works"),
      tags$ul(
        tags$li("Prices come from Yahoo Finance through quantmod::getSymbols(). If Yahoo cannot be reached, the app switches to the Yahoo prices saved in Assignment 5 and says so above the chart."),
        tags$li("Indicators (SMA, EMA, MACD, RSI, standard deviation, linear regression, crossover, crossunder) are the base-R functions written in Assignment 5."),
        tags$li("Moving averages, Bollinger Bands and the trend line are layers on the price chart. Volume, RSI and MACD are panels under it with the same date axis. Each one is switched on or off with the checkboxes."),
        tags$li("Signal = Buy while the rule is bullish, Sell while bearish, otherwise Hold (as in the brief). The chart annotates the bars where the signal changes, which are the actual trades."),
        tags$li("The strategy return assumes buying or selling at the close of the signal bar, with no costs. This is a learning tool, not investment advice.")))
  )
)


# =============================================================================
# SERVER - reactive logic
# =============================================================================

server <- function(input, output, session) {

  # Gather all indicator settings in one list
  params <- reactive({
    list(ma_type = input$ma_type, ma_short = input$ma_short, ma_long = input$ma_long,
         bb_n = input$bb_n, bb_k = input$bb_k,
         rsi_n = input$rsi_n, macd_fast = input$macd_fast, macd_slow = input$macd_slow,
         macd_signal = input$macd_signal)
  })

  # ---- STEP 1: fetch the data for the selected stock ----
  raw <- reactive({
    req(input$symbol, input$date_range)
    validate(need(input$date_range[1] < input$date_range[2],
                  "The start date must be before the end date."))
    res <- withProgress(message = paste("Fetching", input$symbol, "..."), {
      fetch_stock(input$symbol, input$date_range[1] - history_buffer_days,
                  input$date_range[2], input$data_source)
    })
    if (!is.null(res$data) && !is.null(res$message)) {
      showNotification(paste(res$message, "- using the offline CSV instead."),
                       type = "warning", duration = 8)
    }
    validate(need(!is.null(res$data), res$message))
    res
  })

  # ---- STEP 2 + 3 + 4: time frame, indicators, signals, then filter the dates ----
  analysed <- reactive({
    p <- params()
    validate(need(input$ma_short < input$ma_long,
                  "The short-term moving average must be shorter than the long-term one."),
             need(input$macd_fast < input$macd_slow, "MACD fast EMA must be shorter than the slow EMA."))
    x <- apply_time_frame(raw()$data, input$time_frame)
    d <- add_indicators(x, p)
    d <- generate_signals(d, input$rule, input$rsi_levels[1], input$rsi_levels[2])
    # Filter data based on the date range (after the indicators are calculated)
    filtered_data <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
    validate(need(nrow(filtered_data) >= 2, "Not enough data in this date range - choose a wider range."))
    filtered_data
  })

  results <- reactive(backtest(analysed()))

  # ---- Summary boxes ----
  output$vb_close  <- renderText({ d <- analysed(); sprintf("$%.2f", tail(d$Close, 1)) })
  output$vb_change <- renderText(pct(results()$buy_hold))
  output$vb_signal <- renderText(tail(analysed()$Signal, 1))
  output$vb_rule   <- renderText(names(trading_rules)[trading_rules == input$rule])
  output$vb_strategy <- renderText(pct(results()$strategy))
  output$vb_trades <- renderText(sprintf("%d trades, in the market %.0f%% of the time",
                                         results()$trades, 100 * results()$in_market))
  output$vb_buyhold <- renderText(pct(results()$buy_hold))
  output$source_note <- renderText(sprintf("%s  |  %s  |  %s bars  |  Source: %s",
    input$symbol, input$time_frame, nrow(analysed()), raw()$source))

  # Number of extra panels below the price chart (for the plot height)
  n_panels <- reactive(sum(c("Volume", "RSI", "MACD") %in% input$technical_indicators))

  # ---- Main chart ----
  output$stock_chart <- renderPlot({
    d   <- analysed()
    ind <- input$technical_indicators
    x_limits <- range(d$Date)
    x_scale  <- scale_x_date(limits = as.Date(x_limits + c(-1, 1) * as.numeric(median(diff(d$Date)))),
                             expand = expansion(0), date_labels = "%b %Y")
    base_theme <- theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))

    # Create the price chart
    p <- ggplot(d, aes(x = Date))

    # Optional Buy / Sell zone shading (drawn first so it sits behind the price)
    if (input$show_zones) {
      z <- signal_zones(d)
      p <- p + geom_rect(data = z, inherit.aes = FALSE,
                         aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Signal),
                         alpha = 0.10, show.legend = FALSE)
    }

    # Chart type (line, candlestick, area, OHLC bars)
    p <- p + price_layers(d, input$chart_type)

    # STEP 3: indicator layers on the price chart (each can be turned on and off)
    overlay_cols <- c()
    if ("Moving Averages" %in% ind) {
      short_lab <- paste0(input$ma_type, " ", input$ma_short)
      long_lab  <- paste0(input$ma_type, " ", input$ma_long)
      p <- p + geom_line(aes(y = MA_short, colour = short_lab), linewidth = 0.8, na.rm = TRUE) +
               geom_line(aes(y = MA_long,  colour = long_lab),  linewidth = 0.8, na.rm = TRUE)
      overlay_cols[short_lab] <- "#2563eb"
      overlay_cols[long_lab]  <- "#f59e0b"
    }
    if ("Bollinger Bands" %in% ind) {
      bb_lab <- paste0("Bollinger (", input$bb_n, ", ", input$bb_k, ")")
      p <- p + geom_ribbon(aes(ymin = BB_lower, ymax = BB_upper), fill = "#a855f7", alpha = 0.08, na.rm = TRUE) +
               geom_line(aes(y = BB_upper, colour = bb_lab), linewidth = 0.4, linetype = "dashed", na.rm = TRUE) +
               geom_line(aes(y = BB_lower, colour = bb_lab), linewidth = 0.4, linetype = "dashed", na.rm = TRUE)
      overlay_cols[bb_lab] <- "#9333ea"
    }
    if ("Trend Line" %in% ind) {
      n_trend <- min(input$trend_n, nrow(d))
      tr <- linreg(d$Close, n_trend, 0)
      trend_df <- data.frame(Date = tail(d$Date, n_trend), Trend = tr$predicted_values)
      tr_lab <- sprintf("Trend, last %d bars (slope %+.2f/bar)", n_trend, tr$slope)
      p <- p + geom_line(data = trend_df, aes(x = Date, y = Trend, colour = tr_lab), linewidth = 1.2)
      overlay_cols[tr_lab] <- "#0f766e"
    }

    # STEP 4: annotate the bars with the trading signals
    if (input$label_all) {
      p <- p + geom_text(aes(y = High, label = Signal,
                             colour = paste("Signal:", Signal)),
                         angle = 90, hjust = -0.15, size = 2.6, show.legend = FALSE)
      overlay_cols[c("Signal: Buy", "Signal: Sell", "Signal: Hold")] <- c(up_colour, down_colour, "grey55")
    }
    if (input$show_signals) {
      buys  <- d[d$Action == "Buy", ]
      sells <- d[d$Action == "Sell", ]
      offset <- 0.03 * diff(range(c(d$Low, d$High)))
      p <- p +
        geom_point(data = buys,  aes(y = Low - offset),  shape = 24, size = 3.5, fill = up_colour, colour = "white") +
        geom_point(data = sells, aes(y = High + offset), shape = 25, size = 3.5, fill = down_colour, colour = "white") +
        geom_label(data = buys,  aes(y = Low - 2.6 * offset, label = "Buy"), fill = up_colour, colour = "white",
                   size = 3.2, fontface = "bold", linewidth = 0, label.padding = unit(0.12, "lines")) +
        geom_label(data = sells, aes(y = High + 2.6 * offset, label = "Sell"), fill = down_colour, colour = "white",
                   size = 3.2, fontface = "bold", linewidth = 0, label.padding = unit(0.12, "lines"))
    }

    # One colour scale and one fill scale for everything on the price chart.
    # The legend lists only the indicator overlays.
    p <- p +
      scale_colour_manual(values = c(overlay_cols, Up = up_colour, Down = down_colour),
                          breaks = names(overlay_cols)[!grepl("^Signal:", names(overlay_cols))]) +
      scale_fill_manual(values = c(Up = up_colour, Down = down_colour, Buy = up_colour, Sell = down_colour),
                        guide = "none")
    # Extra room above the bars so the per-bar labels are not cut off
    if (input$label_all) p <- p + scale_y_continuous(expand = expansion(mult = c(0.04, 0.12)))

    p <- p + x_scale + base_theme +
      labs(title = sprintf("%s - %s %s chart", input$symbol, input$time_frame, input$chart_type),
           subtitle = sprintf("Rule: %s  |  %d trades in range", names(trading_rules)[trading_rules == input$rule],
                              sum(d$Action != "Hold")),
           x = NULL, y = "Price (USD)")

    panels <- list(p)

    # Volume panel
    if ("Volume" %in% ind) {
      v <- ggplot(d, aes(x = Date, y = Volume / 1e6, fill = Close >= Open)) +
        geom_col(width = 0.8 * as.numeric(median(diff(d$Date))), show.legend = FALSE) +
        scale_fill_manual(values = c(`TRUE` = up_colour, `FALSE` = down_colour)) +
        x_scale + base_theme + labs(x = NULL, y = "Volume (M)")
      panels <- c(panels, list(v))
    }

    # RSI panel
    if ("RSI" %in% ind) {
      r <- ggplot(d, aes(x = Date, y = RSI)) +
        annotate("rect", xmin = x_limits[1], xmax = x_limits[2], ymin = input$rsi_levels[1], ymax = input$rsi_levels[2],
                 fill = "#a855f7", alpha = 0.06) +
        geom_hline(yintercept = input$rsi_levels, linetype = "dashed", colour = "grey45") +
        geom_line(colour = "#7c3aed", linewidth = 0.7, na.rm = TRUE) +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, input$rsi_levels, 100)) +
        x_scale + base_theme + labs(x = NULL, y = paste0("RSI (", input$rsi_n, ")"))
      panels <- c(panels, list(r))
    }

    # MACD panel
    if ("MACD" %in% ind) {
      m <- ggplot(d, aes(x = Date)) +
        geom_col(aes(y = MACD_hist, fill = MACD_hist >= 0),
                 width = 0.8 * as.numeric(median(diff(d$Date))), show.legend = FALSE) +
        scale_fill_manual(values = c(`TRUE` = "#86efac", `FALSE` = "#fca5a5")) +
        geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.7) +
        geom_line(aes(y = MACD_signal, colour = "Signal"), linewidth = 0.7) +
        geom_hline(yintercept = 0, colour = "grey50") +
        scale_colour_manual(values = c(MACD = "#2563eb", Signal = "#ea580c")) +
        x_scale + base_theme + theme(legend.position = "right") +
        labs(x = NULL, y = sprintf("MACD (%d,%d,%d)", input$macd_fast, input$macd_slow, input$macd_signal))
      panels <- c(panels, list(m))
    }

    # Stack the panels so they share the same date axis
    heights <- c(3, rep(1, length(panels) - 1))
    print(wrap_plots(panels, ncol = 1, heights = heights))
  }, height = function() 520 + 170 * n_panels(), res = 96)

  # ---- Signals table ----
  output$signal_table <- renderTable({
    d <- analysed()
    t <- d[d$Action != "Hold", c("Date", "Close", "MA_short", "MA_long", "RSI", "MACD", "Signal", "Action")]
    validate(need(nrow(t) > 0, "No Buy or Sell signals in this range with the current rule and settings."))
    t <- t[order(t$Date, decreasing = TRUE), ]
    t$Date <- format(t$Date)
    names(t)[3:4] <- c(paste0(input$ma_type, input$ma_short), paste0(input$ma_type, input$ma_long))
    t
  }, digits = 2, striped = TRUE, hover = TRUE)

  # ---- Portfolio overview (all stocks with the same settings) ----
  portfolio_data <- reactive({
    p <- params()
    out <- list()
    withProgress(message = "Loading portfolio ...", value = 0, {
      for (s in portfolio) {
        incProgress(1 / length(portfolio), detail = s)
        res <- fetch_stock(s, input$date_range[1] - history_buffer_days, input$date_range[2], input$data_source)
        if (is.null(res$data)) next
        d <- add_indicators(apply_time_frame(res$data, input$time_frame), p)
        d <- generate_signals(d, input$rule, input$rsi_levels[1], input$rsi_levels[2])
        d <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
        if (nrow(d) < 2) next
        out[[s]] <- d
      }
    })
    validate(need(length(out) > 0, "Could not load any portfolio data."))
    out
  })

  output$portfolio_table <- renderTable({
    rows <- lapply(names(portfolio_data()), function(s) {
      d <- portfolio_data()[[s]]
      bt <- backtest(d)
      trades <- d[d$Action != "Hold", ]
      data.frame(Stock = s,
                 `Last close` = sprintf("$%.2f", tail(d$Close, 1)),
                 Change = pct(bt$buy_hold),
                 RSI = sprintf("%.1f", tail(d$RSI, 1)),
                 `Current signal` = tail(d$Signal, 1),
                 `Last trade` = if (nrow(trades)) paste(tail(trades$Action, 1), format(tail(trades$Date, 1))) else "-",
                 Trades = bt$trades,
                 `Strategy return` = pct(bt$strategy),
                 check.names = FALSE)
    })
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE)

  output$portfolio_chart <- renderPlot({
    df <- do.call(rbind, lapply(names(portfolio_data()), function(s) {
      d <- portfolio_data()[[s]]
      data.frame(Date = d$Date, Stock = s, Index = 100 * d$Close / d$Close[1])
    }))
    ggplot(df, aes(Date, Index, colour = Stock)) +
      geom_hline(yintercept = 100, colour = "grey60") +
      geom_line(linewidth = 0.8) +
      theme_minimal(base_size = 13) + theme(legend.position = "top", legend.title = element_blank()) +
      labs(title = "Portfolio performance (start of range = 100)", x = NULL, y = "Index")
  }, res = 96)

  # ---- Data tab ----
  output$data_info <- renderText({
    r <- raw()
    paste0("Symbol: ", input$symbol,
           "\nSource: ", r$source,
           "\nDaily rows fetched (including history for the indicators): ", nrow(r$data),
           "\nFirst / last date fetched: ", format(start(r$data)), " / ", format(end(r$data)),
           "\nMissing values filled: ", attr(r$data, "missing_fixed"),
           "\nRows shown after time frame and date filter: ", nrow(analysed()))
  })
  output$data_tail <- renderTable({
    d <- tail(analysed(), 10)
    d$Date <- format(d$Date)
    d[, c("Date", "Open", "High", "Low", "Close", "Volume", "Signal", "Action")]
  }, digits = 2, striped = TRUE)
}

shinyApp(ui, server)
