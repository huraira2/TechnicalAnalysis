# =============================================================================
# stdev.R - Standard Deviation (population)
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# sigma = sqrt( sum((x_i - mean)^2) / n )
# Note: divides by n (population), as in the assignment formula. R's built-in
# sd() divides by n - 1 (sample), so the two differ slightly.
# =============================================================================

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  stdev_result <- stdev(data)
  print(stdev_result)   # 4.946629
}
