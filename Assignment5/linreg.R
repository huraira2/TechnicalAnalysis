# =============================================================================
# linreg.R - Linear Regression (least squares line through recent data)
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# Fits y = intercept + slope * x on a window at the end of regressionSource,
# where x = 1, 2, ..., (number of points in the window).
#   slope     = sum((x - mean_x) * (y - mean_y)) / sum((x - mean_x)^2)
#   intercept = mean_y - slope * mean_x
# The window is the last regressionLength points, shifted back by
# regressionOffset points:
#   end_index   = min(n, n - regressionOffset)
#   start_index = max(1, end_index - regressionLength + 1)
# =============================================================================

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the ending index: 'regressionOffset' points back from the end
  end_index <- min(n, n - regressionOffset)

  # Calculate the starting index so the window holds exactly regressionLength
  # points. (The pseudocode used n - regressionLength + regressionOffset, which
  # gives regressionLength + 1 points when the offset is 0 and a shorter window
  # as the offset grows. This version keeps the window the requested size.)
  start_index <- max(1, end_index - regressionLength + 1)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- seq_len(length(source_subset))

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(slope            = slope,
                 intercept        = intercept,
                 predicted_values = predicted_values)
  return(result)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  linreg_result <- linreg(data, regressionLength = 5, regressionOffset = 0)
  print(linreg_result)
}
