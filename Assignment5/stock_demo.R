# =============================================================================
# stock_demo.R - apply all nine indicators to the portfolio stocks
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only (read.csv, base graphics).
#
# Run from the Assignment5 folder:   Rscript stock_demo.R
# Reads stock_prices.csv (made by get_stock_data.R), prints a summary table of
# the latest indicator values and the buy/sell signals for each stock, and
# saves one chart per stock in the charts/ folder.
# =============================================================================

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
            "stoch_rsi.R", "crossover.R", "crossunder.R")) {
  source(f)
}

prices <- read.csv("stock_prices.csv", stringsAsFactors = FALSE)
prices$Date <- as.Date(prices$Date)
symbols <- unique(prices$Symbol)
dir.create("charts", showWarnings = FALSE)

# sma() returns only complete windows; pad the front with NA so it lines up
# with the dates for plotting and for crossover().
pad_front <- function(values, total_length) {
  c(rep(NA, total_length - length(values)), values)
}

summary_rows <- list()

for (s in symbols) {
  d     <- prices[prices$Symbol == s, ]
  close <- d$Close
  n     <- length(close)

  sma20  <- pad_front(sma(close, 20), n)
  sma50  <- pad_front(sma(close, 50), n)
  ema20  <- ema(close, 20)
  m      <- macd(close, 12, 26, 9)
  rsi14  <- rsi(close, 14)
  srsi   <- stoch_rsi(close, 14, 3, 3)
  vol20  <- stdev(tail(close, 20))                 # last 20 days
  trend  <- linreg(close, 20, 0)                   # last 20 days

  golden <- crossover(sma20, sma50)                # 20-day vs 50-day SMA
  macd_x <- crossover(m$macd_line, m$signal_line)  # MACD vs signal line
  below  <- crossunder(close, sma50)               # price drops below SMA50

  cat(sprintf("\n================ %s (%d days, %s to %s) ================\n",
              s, n, format(min(d$Date)), format(max(d$Date))))
  cat(sprintf("Last close %.2f | SMA20 %.2f | SMA50 %.2f | EMA20 %.2f\n",
              close[n], sma20[n], sma50[n], ema20[n]))
  cat(sprintf("MACD %.3f | Signal %.3f | Histogram %.3f\n",
              m$macd_line[n], m$signal_line[n], m$histogram[n]))
  cat(sprintf("RSI14 %.1f | StochRSI %%K %.2f | %%D %.2f\n",
              rsi14[n], tail(srsi$k_line, 1), tail(srsi$d_line, 1)))
  cat(sprintf("20-day st. dev. %.2f | 20-day trend slope %.3f per day\n",
              vol20, trend$slope))
  cat("SMA20/SMA50 crossovers:\n")
  for (i in which(golden != "None")) {
    cat(sprintf("   %s  %s  (%s)\n", format(d$Date[i]), golden[i],
                ifelse(golden[i] == "Up", "buy signal", "sell signal")))
  }
  cat(sprintf("MACD crosses: %d up, %d down | days closing below SMA50 (crossunder): %d\n",
              sum(macd_x == "Up"), sum(macd_x == "Down"), sum(below == "True")))

  summary_rows[[s]] <- data.frame(
    Symbol     = s,
    Close      = round(close[n], 2),
    SMA20      = round(sma20[n], 2),
    SMA50      = round(sma50[n], 2),
    RSI14      = round(rsi14[n], 1),
    MACD_Hist  = round(m$histogram[n], 3),
    StDev20    = round(vol20, 2),
    Slope20    = round(trend$slope, 3),
    Golden_Up  = sum(golden == "Up"),
    Golden_Dn  = sum(golden == "Down")
  )

  # ------------------------------------------------------------- chart
  png(file.path("charts", paste0(s, "_indicators.png")), width = 1000, height = 900, res = 110)
  par(mfrow = c(3, 1), mar = c(3, 4.5, 2.5, 1))

  plot(d$Date, close, type = "l", col = "grey30", lwd = 1.2,
       main = paste(s, "- close with SMA20, SMA50, EMA20 and 20-day trend"),
       xlab = "", ylab = "Price (USD)")
  lines(d$Date, sma20, col = "blue", lwd = 1.5)
  lines(d$Date, sma50, col = "red", lwd = 1.5)
  lines(d$Date, ema20, col = "darkgreen", lty = 2)
  lines(tail(d$Date, 20), trend$predicted_values, col = "orange", lwd = 3)
  up   <- which(golden == "Up")
  down <- which(golden == "Down")
  points(d$Date[up],   close[up],   pch = 24, bg = "green3", cex = 1.6)
  points(d$Date[down], close[down], pch = 25, bg = "red",    cex = 1.6)
  legend("topleft", bty = "n", cex = 0.8,
         legend = c("Close", "SMA20", "SMA50", "EMA20", "Trend (linreg)", "Up cross", "Down cross"),
         col = c("grey30", "blue", "red", "darkgreen", "orange", "black", "black"),
         lty = c(1, 1, 1, 2, 1, NA, NA), lwd = c(1, 1.5, 1.5, 1, 3, NA, NA),
         pch = c(NA, NA, NA, NA, NA, 24, 25), pt.bg = c(NA, NA, NA, NA, NA, "green3", "red"))

  plot(d$Date, m$macd_line, type = "l", col = "blue",
       ylim = range(c(m$macd_line, m$signal_line, m$histogram)),
       main = "MACD (12, 26, 9)", xlab = "", ylab = "MACD")
  lines(d$Date, m$signal_line, col = "red")
  segments(d$Date, 0, d$Date, m$histogram, col = ifelse(m$histogram >= 0, "grey50", "grey75"))
  abline(h = 0, lty = 3)
  legend("topleft", bty = "n", cex = 0.8, legend = c("MACD line", "Signal line", "Histogram"),
         col = c("blue", "red", "grey50"), lty = 1)

  plot(d$Date, rsi14, type = "l", col = "purple", ylim = c(0, 100),
       main = "RSI (14) and Stochastic RSI %K (x100)", xlab = "", ylab = "Value")
  lines(d$Date, pad_front(srsi$k_line, n) * 100, col = "darkcyan", lty = 2)
  abline(h = c(30, 70), lty = 3, col = "grey40")
  legend("topleft", bty = "n", cex = 0.8, legend = c("RSI 14", "StochRSI %K x100", "30 / 70 levels"),
         col = c("purple", "darkcyan", "grey40"), lty = c(1, 2, 3))
  dev.off()
}

cat("\n================ Portfolio summary (latest day) ================\n")
summary_table <- do.call(rbind, summary_rows)
rownames(summary_table) <- NULL
print(summary_table)
write.csv(summary_table, "indicator_summary.csv", row.names = FALSE)
cat("\nSaved indicator_summary.csv and charts/<SYMBOL>_indicators.png\n")
