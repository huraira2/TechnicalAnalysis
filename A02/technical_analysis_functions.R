# =============================================================================
# technical_analysis_functions.R
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# Author : Nadeeka (Student ID: 664430944)
#
# Utility functions to:
#   1. Read the portfolio (portfolio.txt)
#   2. Import daily stock prices from Yahoo Finance with quantmod
#   3. Compute basic statistics (moving average, mean, mode, median, sd)
#   4. Display the imported data and statistics (tables + charts)
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------
required_packages <- c("quantmod", "TTR", "xts", "zoo")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, library, character.only = TRUE))
}
install_if_missing(required_packages)

# ---- 1. Read the portfolio file ----------------------------------------------
#' Read stock symbols from a text file (one symbol per line).
#' Blank lines and lines starting with '#' are ignored.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("Portfolio file not found: ", file)
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  unique(toupper(symbols))
}

# ---- 2. Import stock data ----------------------------------------------------
#' Download daily OHLCV data for every symbol in the portfolio.
#' Returns a named list with one data frame per stock symbol.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to   = Sys.Date()) {
  symbols <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    result <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) { warning("Could not load ", sym, ": ", conditionMessage(e)); NULL }
    )
    if (is.null(result)) next

    # Convert the xts object into a plain data frame with clean column names
    df <- data.frame(Date = index(result), coredata(result), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df$Symbol <- sym
    stock_list[[sym]] <- df
  }
  message("Loaded ", length(stock_list), " of ", length(symbols), " symbols.")
  stock_list
}

# ---- 3. Statistics -----------------------------------------------------------
#' Mode of a numeric vector. Prices are continuous, so values are rounded
#' to 'digits' decimals first; ties return the first (lowest) value.
calc_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

#' Calculate statistics for ONE stock data frame.
#' Returns a list with:
#'   $summary        - one-row data frame (mean, median, mode, sd, ...)
#'   $moving_average - data frame of Date, Close, SMA_20, SMA_50
calculate_statistics <- function(stock_df, price_col = "Close",
                                 short_n = 20, long_n = 50) {
  price <- stock_df[[price_col]]

  ma <- data.frame(
    Date   = stock_df$Date,
    Close  = price,
    SMA_20 = as.numeric(SMA(price, n = short_n)),
    SMA_50 = as.numeric(SMA(price, n = long_n))
  )

  summary <- data.frame(
    Symbol        = stock_df$Symbol[1],
    Days          = length(price),
    Start_Date    = min(stock_df$Date),
    End_Date      = max(stock_df$Date),
    Mean          = round(mean(price, na.rm = TRUE), 2),
    Median        = round(median(price, na.rm = TRUE), 2),
    Mode          = calc_mode(price),
    Std_Dev       = round(sd(price, na.rm = TRUE), 2),
    Min           = round(min(price, na.rm = TRUE), 2),
    Max           = round(max(price, na.rm = TRUE), 2),
    Last_Close    = round(tail(price, 1), 2),
    Last_SMA_20   = round(tail(ma$SMA_20, 1), 2),
    Last_SMA_50   = round(tail(ma$SMA_50, 1), 2),
    Avg_Volume    = round(mean(stock_df$Volume, na.rm = TRUE)),
    row.names = NULL
  )

  list(summary = summary, moving_average = ma)
}

#' Apply calculate_statistics() to every stock and combine the summaries.
calculate_portfolio_statistics <- function(stock_list) {
  stats <- lapply(stock_list, calculate_statistics)
  list(
    by_stock = stats,
    summary  = do.call(rbind, lapply(stats, `[[`, "summary"))
  )
}

# ---- 4. Display utilities ----------------------------------------------------
#' Print a header plus the first and last n rows of a stock data frame.
display_stock_data <- function(stock_df, n = 5) {
  sym <- stock_df$Symbol[1]
  cat("\n==============================================\n")
  cat(" Stock:", sym, "|", nrow(stock_df), "trading days |",
      format(min(stock_df$Date)), "to", format(max(stock_df$Date)), "\n")
  cat("==============================================\n")
  cat("First", n, "rows:\n");  print(head(stock_df[, 1:7], n), row.names = FALSE)
  cat("\nLast", n, "rows:\n"); print(tail(stock_df[, 1:7], n), row.names = FALSE)
}

#' Yahoo-Finance style "quote summary" for the latest trading day.
display_quote_summary <- function(stock_df) {
  last <- tail(stock_df, 1)
  prev <- tail(stock_df, 2)[1, ]
  change     <- last$Close - prev$Close
  change_pct <- 100 * change / prev$Close
  wk52 <- range(stock_df$Low, stock_df$High)
  data.frame(
    Symbol       = last$Symbol,
    Date         = last$Date,
    Close        = round(last$Close, 2),
    Change       = sprintf("%+.2f", change),
    `Change (%)` = sprintf("%+.2f%%", change_pct),
    `Day Range`  = sprintf("%.2f - %.2f", last$Low, last$High),
    `52-Wk Range`= sprintf("%.2f - %.2f", wk52[1], wk52[2]),
    Volume       = format(last$Volume, big.mark = ","),
    check.names  = FALSE
  )
}

#' Candlestick chart with volume and 20/50-day moving averages (quantmod).
plot_stock_chart <- function(stock_df) {
  x <- xts(stock_df[, c("Open", "High", "Low", "Close", "Volume")],
           order.by = stock_df$Date)
  chartSeries(x, name = stock_df$Symbol[1], theme = chartTheme("white"),
              TA = "addVo(); addSMA(n = 20, col = 'blue'); addSMA(n = 50, col = 'red')")
}

#' Base-R line chart of the closing price and the two moving averages.
plot_moving_averages <- function(ma_df, symbol) {
  plot(ma_df$Date, ma_df$Close, type = "l", col = "grey30", lwd = 1.5,
       main = paste(symbol, "- Close with 20 & 50 day SMA"),
       xlab = "Date", ylab = "Price (USD)")
  lines(ma_df$Date, ma_df$SMA_20, col = "blue", lwd = 1.5)
  lines(ma_df$Date, ma_df$SMA_50, col = "red",  lwd = 1.5)
  legend("topleft", legend = c("Close", "SMA 20", "SMA 50"),
         col = c("grey30", "blue", "red"), lwd = 1.5, bty = "n")
}

#' Compare all stocks on one chart, rebased to 100 on the first day.
plot_normalized_performance <- function(stock_list) {
  norm <- lapply(stock_list, function(d) 100 * d$Close / d$Close[1])
  cols <- seq_along(stock_list) + 1
  plot(stock_list[[1]]$Date, norm[[1]], type = "n",
       ylim = range(unlist(norm)), xlab = "Date", ylab = "Index (start = 100)",
       main = "Portfolio performance (rebased to 100)")
  for (i in seq_along(norm)) lines(stock_list[[i]]$Date, norm[[i]], col = cols[i], lwd = 1.5)
  abline(h = 100, lty = 2, col = "grey60")
  legend("topleft", legend = names(stock_list), col = cols, lwd = 1.5, bty = "n")
}

# ---- 5. Main (runs only when the script is executed directly) ----------------
if (sys.nframe() == 0) {
  stocks <- load_stock_data("portfolio.txt")

  for (s in stocks) display_stock_data(s)

  stats <- calculate_portfolio_statistics(stocks)
  cat("\n===== Summary statistics (Close price) =====\n")
  print(stats$summary, row.names = FALSE)

  cat("\n===== Latest quote summary =====\n")
  print(do.call(rbind, lapply(stocks, display_quote_summary)), row.names = FALSE)

  dir.create("output", showWarnings = FALSE)
  write.csv(stats$summary, "output/summary_statistics.csv", row.names = FALSE)
  for (s in names(stocks)) write.csv(stocks[[s]], file.path("output", paste0(s, ".csv")), row.names = FALSE)
}
