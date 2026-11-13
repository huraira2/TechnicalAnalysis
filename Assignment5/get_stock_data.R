# =============================================================================
# get_stock_data.R - download the portfolio prices once and save them as CSV
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
#
# Builds on the earlier stage (Assignment2/portfolio.txt): AAPL, MSFT, GOOGL,
# AMZN, TSLA, same fixed dates (2025-08-24 to 2026-08-24).
# quantmod is used ONLY to download data. The indicators themselves are base R.
# The CSV is already in the repo, so you only need this to refresh the data.
#
# Run from the Assignment5 folder:   Rscript get_stock_data.R
# =============================================================================

library(quantmod)

symbols <- scan("../Assignment2/portfolio.txt", what = character(), quiet = TRUE)
from    <- as.Date("2025-08-24")
to      <- as.Date("2026-08-24")

all_prices <- NULL
for (s in symbols) {
  x  <- getSymbols(s, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  df <- data.frame(Date   = index(x),
                   Symbol = s,
                   Open   = as.numeric(Op(x)),
                   High   = as.numeric(Hi(x)),
                   Low    = as.numeric(Lo(x)),
                   Close  = as.numeric(Cl(x)),
                   Volume = as.numeric(Vo(x)))
  all_prices <- rbind(all_prices, df)
  cat(sprintf("%-5s %d trading days\n", s, nrow(df)))
}

write.csv(all_prices, "stock_prices.csv", row.names = FALSE)
cat("Saved stock_prices.csv with", nrow(all_prices), "rows\n")
