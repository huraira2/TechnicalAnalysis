# =============================================================================
# sma.R - Simple Moving Average
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# SMA = (sum of the data points in the window) / (number of points in window)
# Returns one value per complete window: length(data) - period + 1 values.
# =============================================================================

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_windows  <- length(data) - period + 1
  sma_values <- numeric(n_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    current_window <- data[i:(i + period - 1)]        # the 'period' values
    sma_values[i]  <- sum(current_window) / period    # their mean
  }

  return(sma_values)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  sma_result <- sma(data, period = 3)
  print(sma_result)   # 12.33333 15.66667 17.66667 20.00000 21.66667 23.66667 23.33333
}
