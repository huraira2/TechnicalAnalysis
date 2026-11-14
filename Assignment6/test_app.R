# =============================================================================
# test_app.R - tests the dashboard's server logic without a browser
# BDA400 Assignment 6 | Nadeeka Godage (664430944)
#
# Run from the repository folder or the Assignment6 folder:
#   Rscript Assignment6/test_app.R      (repository)
#   Rscript test_app.R                  (inside the app folder)
# Uses shiny::testServer() to set the widgets, render each chart and check the
# results and the error messages. Charts are saved to a temporary folder.
# =============================================================================

library(shiny)
if (!file.exists("app.R")) {
  # run from the repository folder, or from the folder that holds the unzipped app
  setwd(if (dir.exists("Assignment6")) "Assignment6" else "NadeekaGodage_BDA400_A06")
}
app <- shinyAppDir(".")
chart_dir <- file.path(tempdir(), "dashboard_charts")
dir.create(chart_dir, showWarnings = FALSE)

passed <- 0; failed <- 0
check <- function(name, ok) {
  if (isTRUE(ok)) { passed <<- passed + 1; cat("  PASS ", name, "\n") }
  else            { failed <<- failed + 1; cat("  FAIL ", name, "\n") }
}
error_text <- function(expr) tryCatch({ expr; "no error" }, error = function(e) conditionMessage(e))
save_chart <- function(x, file) {
  writeBin(jsonlite::base64_dec(sub("^data:image/png;base64,", "", x$src)), file)
  file.exists(file) && file.size(file) > 2000
}

all_ind <- c("Moving Averages", "Bollinger Bands", "Trend Line", "Volume", "RSI", "MACD")
cases <- list(
  list(name = "candle_daily_ma",  chart_type = "Candlestick", time_frame = "Daily",   rule = "ma",
       technical_indicators = c("Moving Averages", "RSI", "MACD")),
  list(name = "line_all_macd",    chart_type = "Line",        time_frame = "Daily",   rule = "macd",
       technical_indicators = all_ind, show_zones = TRUE),
  list(name = "area_weekly_rsi",  chart_type = "Area",        time_frame = "Weekly",  rule = "rsi",
       technical_indicators = c("Moving Averages", "RSI")),
  list(name = "ohlc_monthly",     chart_type = "OHLC Bars",   time_frame = "Monthly", rule = "ma_rsi",
       technical_indicators = c("Moving Averages", "Volume"), label_all = TRUE),
  list(name = "candle_no_ind",    chart_type = "Candlestick", time_frame = "Weekly",  rule = "ma",
       technical_indicators = character(0), show_signals = FALSE),
  list(name = "candle_ema_label", chart_type = "Candlestick", time_frame = "Weekly",  rule = "ma",
       technical_indicators = "Moving Averages", label_all = TRUE, ma_type = "EMA", ma_short = 10, ma_long = 30)
)
defaults <- list(symbol = "TSLA", data_source = "Yahoo Finance",
                 date_range = as.Date(c("2025-08-24", "2026-08-24")),
                 show_signals = TRUE, show_zones = FALSE, label_all = FALSE,
                 ma_type = "SMA", ma_short = 20, ma_long = 50, bb_n = 20, bb_k = 2, trend_n = 60,
                 rsi_n = 14, rsi_levels = c(30, 70), macd_fast = 12, macd_slow = 26, macd_signal = 9)

testServer(app, {
  cat("\n== Charts for every chart type, time frame and rule ==\n")
  for (cs in cases) {
    do.call(session$setInputs, modifyList(defaults, cs[names(cs) != "name"]))
    ok <- tryCatch(save_chart(output$stock_chart, file.path(chart_dir, paste0(cs$name, ".png"))),
                   error = function(e) FALSE)
    d <- analysed()
    check(sprintf("%-17s %3d bars, %2d trades, signal %s", cs$name, nrow(d), sum(d$Action != "Hold"),
                  tail(d$Signal, 1)), ok && all(d$Signal %in% c("Buy", "Sell", "Hold")))
  }
  cat("  data source:", raw()$source, "\n")

  cat("\n== Trading rule logic ==\n")
  session$setInputs(rule = "ma", time_frame = "Daily", technical_indicators = "Moving Averages")
  d <- analysed()
  buys <- which(d$Action == "Buy"); sells <- which(d$Action == "Sell")
  check("MA rule: every Buy bar has short MA above long MA (and below the bar before)",
        all(d$MA_short[buys] > d$MA_long[buys]) && all(d$MA_short[buys - 1] <= d$MA_long[buys - 1]))
  check("MA rule: every Sell bar has short MA below long MA",
        all(d$MA_short[sells] < d$MA_long[sells]))
  check("MA rule: Signal matches the brief's ifelse()",
        identical(d$Signal, {s <- ifelse(d$MA_short > d$MA_long, "Buy",
                                         ifelse(d$MA_short < d$MA_long, "Sell", "Hold")); s[is.na(s)] <- "Hold"; s}))
  check("Buy and Sell actions alternate", all(diff(match(d$Action[d$Action != "Hold"], c("Buy", "Sell"))) != 0))
  check("Signals table lists every trade",
        length(gregexpr("<tr", output$signal_table)[[1]]) - 1 == sum(d$Action != "Hold"))
  check("Portfolio table has all 5 stocks",
        all(sapply(c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA"), grepl, output$portfolio_table)))
  check("Portfolio chart renders", save_chart(output$portfolio_chart, file.path(chart_dir, "portfolio.png")))

  cat("\n== Error handling ==\n")
  session$setInputs(symbol = "NOTAREALTICKER123")
  check("Unknown ticker gives a message", grepl("no offline data", error_text(raw())))
  session$setInputs(symbol = "TSLA", ma_short = 60, ma_long = 50)
  check("Short MA >= long MA gives a message", grepl("must be shorter", error_text(analysed())))
  session$setInputs(ma_short = 20, date_range = as.Date(c("2026-08-24", "2025-08-24")))
  check("Start after end gives a message", grepl("start date must be before", error_text(raw())))
  session$setInputs(date_range = as.Date(c("2025-08-24", "2026-08-24")), data_source = "Offline CSV")
  check("Offline CSV source works", grepl("Offline CSV", raw()$source) && nrow(analysed()) > 200)
  session$setInputs(symbol = "NVDA")
  check("Ticker missing from the offline CSV gives a message", grepl("No offline data for NVDA", error_text(raw())))
})

cat(sprintf("\n== RESULT: %d passed, %d failed ==  (charts saved in %s)\n", passed, failed, chart_dir))
if (failed > 0) quit(status = 1)
