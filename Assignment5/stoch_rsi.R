# =============================================================================
# stoch_rsi.R - Stochastic RSI (%K and %D)
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages. Uses rsi() from rsi.R and sma() from sma.R.
#
# StochRSI = (RSI - lowest RSI) / (highest RSI - lowest RSI)   -> 0 to 1
# %K = SMA(StochRSI, k_period),  %D = SMA(%K, d_period)
# As in the pseudocode, lowest/highest are taken over the whole RSI series
# (NA values from the RSI warm-up are ignored). Because sma() returns only
# complete windows, %K is k_period - 1 values shorter than the data and %D is
# a further d_period - 1 values shorter.
# =============================================================================

if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- suppressWarnings(rsi(data, period))

  # Calculate the StochRSI (ignore the NA warm-up values)
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0 || max(valid) == min(valid)) {
    k_values <- rep(NA_real_, length(rsi_values))   # not enough data to scale
  } else {
    min_rsi  <- min(valid)
    max_rsi  <- max(valid)
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  # Period 14 needs more than 14 prices, so with 10 values every result is NA
  stoch_rsi_result <- stoch_rsi(data, period = 14, k_period = 3, d_period = 3)
  print(stoch_rsi_result)
  # With a period that fits the data:
  print(stoch_rsi(data, period = 3, k_period = 3, d_period = 3))
}
