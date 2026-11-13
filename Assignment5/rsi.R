# =============================================================================
# rsi.R - Relative Strength Index (Wilder's smoothing)
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# RSI = 100 - 100 / (1 + RS),  RS = average gain / average loss
# Returns a vector the same length as data; the first 'period' values are NA.
# Follows the pseudocode, with three fixes:
#   * gains and losses start as 0 (otherwise unfilled positions would be NA)
#   * if the average loss is 0, RSI = 100 (avoids dividing by zero)
#   * the first RSI value uses the starting averages without re-smoothing
#     (see the comment in the loop)
# =============================================================================

rsi <- function(data, period) {
  n <- length(data)

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)
  if (n <= period) {
    warning("Not enough data points for RSI: need more than 'period' values")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[-1] - data[-n]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using the Wilder's smoothing method.
  # The first RSI (at point period + 1) uses the starting averages directly;
  # after that each new gain/loss is blended in. (The pseudocode applied the
  # smoothing at period + 1 as well, which counts gains[period] twice; this
  # version matches the standard Wilder RSI and TTR::RSI.)
  for (i in (period + 1):n) {
    if (i > period + 1) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    }

    rsi_values[i] <- rsi_from_averages(avg_gain, avg_loss)
  }

  return(rsi_values)
}

# RSI = 100 - 100 / (1 + RS); if there were no losses RSI is 100
rsi_from_averages <- function(avg_gain, avg_loss) {
  if (avg_loss == 0) {
    return(100)
  }
  rs <- avg_gain / avg_loss
  return(100 - (100 / (1 + rs)))
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  rsi_result <- rsi(data, period = 5)
  print(rsi_result)
}
