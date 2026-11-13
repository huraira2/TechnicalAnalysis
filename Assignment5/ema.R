# =============================================================================
# ema.R - Exponential Moving Average
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1),  multiplier = 2/(period+1)
# The first EMA value is the first data point (as in the pseudocode).
# Returns a vector the same length as data.
# =============================================================================

# Exponential Moving Average (EMA) function
ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  ema_result <- ema(data, period = 3)
  print(ema_result)   # 10.000 11.000 13.000 16.500 17.250 19.625 22.3125 23.15625 22.07813
}
