# =============================================================================
# crossunder.R - Crossunder signals between two series
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
# Base R only - no packages.
#
# At each point i (from 2):
#   "True"  if arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]  (arr1 crosses below)
#   "False" otherwise. The first point is "None" (no previous point).
# Output follows the pseudocode (character vector). Points where any value is
# NA (e.g. indicator warm-up) are "False".
# =============================================================================

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("False", length(arr1))
  if (length(arr1) >= 1) crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      values <- c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1])
      if (any(is.na(values))) next                           # leave as "False"
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}

# Example (runs only when this file is executed directly)
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossunder_signals <- crossunder(arr1, arr2)
  print(crossunder_signals)  # "None" then "False" x 8 (arr1 never crosses below arr2)
}
