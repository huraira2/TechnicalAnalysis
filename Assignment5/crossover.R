# =============================================================================
# crossover.R - Crossover signals between two series
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# At each point i (from 2):
#   "Up"   if arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1]  (arr1 crosses above)
#   "Down" if arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]  (arr1 crosses below)
#   "None" otherwise. The first point is always "None".
# Output follows the pseudocode (character vector). Points where any value is
# NA (e.g. indicator warm-up) are "None".
# =============================================================================

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      values <- c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1])
      if (any(is.na(values))) next                           # leave as "None"
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossover_signals <- crossover(arr1, arr2)
  print(crossover_signals)   # "None" "None" "None" "Up" "None" "None" "None" "None" "None"
}
