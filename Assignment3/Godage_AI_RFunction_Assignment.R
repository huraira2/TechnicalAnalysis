# =============================================================================
# Godage_AI_RFunction_Assignment.R
# BDA400 - Data Science Tools and Techniques
# Assignment 3: Prompting R Functions with AI
# Author : Nadeeka Godage (Student ID: 664430944)
# Date   : November 13, 2026
# R      : 4.6.1 (RStudio 2026.08.1)
#
# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5) for drafting
# the R function and its comments through the prompt sequence listed below.
# Prompts used: see the PROMPT LOG section of this file and the Appendix in
# Godage_Reflection.docx. I verified outputs using manual calculation of the
# quartiles and fences, comparison with base R boxplot.stats() on 1,000 random
# synthetic vectors, and edge-case tests. All final calculations are done by
# myself. I am responsible for the accuracy and originality of this work.
# =============================================================================
#
# ORIGINAL TASK DESCRIPTION (one sentence)
# -----------------------------------------------------------------------------
# "Remove outliers from a numeric vector using the 1.5 x IQR rule and report
#  which values were removed and the limits that were used."
#
# PROMPT LOG (initial and revised prompts, in order)
# -----------------------------------------------------------------------------
# P1 (seed)    : "Write an R function that removes outliers from a numeric
#                 vector using the 1.5 x IQR rule and reports which values were
#                 removed and the limits that were used."
# P2 (refine)  : "Revise the function to include comments, argument validation,
#                 and return a clean result."
# P3 (explain) : "Explain what each line of the R code does and how you might
#                 test if it works correctly."
# P4 (debug)   : "Fix this error in the R function: Error in quantile.default(x,
#                 0.25): missing values and NaN's not allowed if 'na.rm' is
#                 FALSE."
# P5 (debug)   : "My function gives different results from boxplot.stats() for
#                 c(64, 57, 53, 47, 55, 45, 47, 61, 54, 52, 25, 59, 63, 41, 17,
#                 50, 44, 40, 50, 57, 65, 31). It removes 25 and 17 but
#                 boxplot.stats() only flags 17. Why, and how do I make them
#                 match?"
# P6 (optimize): "Optimize the function for efficiency and readability."
# P7 (testing) : "How could you test this function with edge cases?"
# P8 (summary) : "Summarize how this R function changed across revisions.
#                 Highlight what improvements were human-driven vs.
#                 AI-generated."
#
# VERSION HISTORY
# -----------------------------------------------------------------------------
# v1  AI output for P1.            Worked on clean data only.
# v2  AI output for P2 + P4.       Added validation and NA handling.
# v3  FINAL, after P5-P7 + my own  Tukey hinges (matches boxplot.stats),
#     changes.                     configurable k, a tidy list result, and a
#                                  full test suite.
# =============================================================================


# =============================================================================
# VERSION 1 - AI-generated function (response to seed prompt P1)
# Kept unchanged so the evolution can be seen. Comments are mine.
# =============================================================================
remove_outliers_v1 <- function(x) {
  q1 <- quantile(x, 0.25)            # 1st quartile (25th percentile)
  q3 <- quantile(x, 0.75)            # 3rd quartile (75th percentile)
  iqr <- q3 - q1                     # interquartile range
  lower <- q1 - 1.5 * iqr            # lower fence
  upper <- q3 + 1.5 * iqr            # upper fence
  x[x >= lower & x <= upper]         # keep only the values inside the fences
}
# Problems found when I tested v1 (see the test log):
#   * c(10, 15, NA, 999, 20, 25) -> Error: missing values and NaN's not allowed
#   * c("a", "b")                -> Error: non-numeric argument to binary operator
#   * The task asked for the removed values and limits, but v1 only returns the
#     kept values, so the "report" part of the task was ignored.
#   * Results sometimes differ from boxplot.stats() (quantile type 7 vs Tukey).


# =============================================================================
# VERSION 2 - AI revision after prompts P2 and P4
# =============================================================================
remove_outliers_v2 <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) stop("x must be a numeric vector")      # type check
  if (na.rm) x <- x[!is.na(x)]                                 # drop NA values
  q <- quantile(x, c(0.25, 0.75), names = FALSE)               # Q1 and Q3
  iqr <- q[2] - q[1]                                           # IQR
  lower <- q[1] - 1.5 * iqr                                    # lower fence
  upper <- q[2] + 1.5 * iqr                                    # upper fence
  keep <- x >= lower & x <= upper                              # inside fences?
  list(clean = x[keep], outliers = x[!keep],                   # tidy result
       lower = lower, upper = upper)
}
# v2 fixed the NA and character errors and now reports the outliers, but my
# comparison test still found vectors where it disagrees with boxplot.stats().
# Reason: quantile() uses "type 7" interpolation by default, while
# boxplot.stats() uses Tukey's hinges from fivenum(). For small samples the two
# methods give different quartiles, so the fences (and the outliers) differ.


# =============================================================================
# VERSION 3 - FINAL function (after P5-P7 and my own edits)
# =============================================================================
#' Remove outliers from a numeric vector using the k x IQR rule
#'
#' @param x       Numeric vector.
#' @param k       Fence multiplier (default 1.5 = Tukey's rule; 3 = "extreme").
#' @param method  "tukey" (default) uses Tukey's hinges, the same as
#'                boxplot.stats(); "quantile" uses quantile() type 7.
#' @param na.rm   If TRUE (default) missing values are dropped before the check.
#' @return A list with:
#'   clean    - values kept (original order)
#'   outliers - values removed (original order)
#'   lower, upper - the fences used
#'   n_na     - number of missing values dropped
remove_outliers <- function(x, k = 1.5, method = c("tukey", "quantile"),
                            na.rm = TRUE) {

  # --- 1. Argument validation ------------------------------------------------
  if (!is.numeric(x)) {                                  # must be numbers
    stop("`x` must be a numeric vector, not ", class(x)[1], ".", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k <= 0) {
    stop("`k` must be a single positive number.", call. = FALSE)
  }
  method <- match.arg(method)                            # "tukey" or "quantile"

  # --- 2. Missing values -----------------------------------------------------
  is_missing <- is.na(x)                                 # TRUE for NA and NaN
  n_na <- sum(is_missing)                                # count them
  if (n_na > 0 && !na.rm) {
    stop("`x` contains ", n_na, " missing value(s); use na.rm = TRUE.",
         call. = FALSE)
  }
  x <- x[!is_missing]                                    # work on complete data
  if (length(x) == 0) {
    stop("`x` has no non-missing values.", call. = FALSE)
  }

  # --- 3. Quartiles ----------------------------------------------------------
  quartiles <- if (method == "tukey") {
    stats::fivenum(x)[c(2, 4)]                           # lower and upper hinge
  } else {
    stats::quantile(x, c(0.25, 0.75), names = FALSE)     # type 7 quartiles
  }

  # --- 4. Fences -------------------------------------------------------------
  iqr   <- diff(quartiles)                               # Q3 - Q1
  lower <- quartiles[1] - k * iqr                        # lower fence
  upper <- quartiles[2] + k * iqr                        # upper fence

  # --- 5. Split the data (one vectorised comparison) -------------------------
  keep <- x >= lower & x <= upper                        # TRUE = not an outlier

  # --- 6. Clean result -------------------------------------------------------
  list(
    clean    = x[keep],                                  # values kept
    outliers = x[!keep],                                 # values removed
    lower    = lower,                                    # lower fence
    upper    = upper,                                    # upper fence
    n_na     = n_na                                      # NAs dropped
  )
}


# =============================================================================
# TESTS - run with source("Godage_AI_RFunction_Assignment.R", echo = TRUE)
# Only synthetic data is used.
# =============================================================================

# ---- Test 1: the example vector from the assignment -------------------------
sample_data <- c(10, 15, 999, 20, 25)
result <- remove_outliers(sample_data)
result
# Manual check: sorted = 10 15 20 25 999; Tukey hinges Q1 = 15, Q3 = 25
# IQR = 10, lower = 15 - 15 = 0, upper = 25 + 15 = 40  ->  999 is the outlier.
stopifnot(identical(result$outliers, 999),
          result$lower == 0, result$upper == 40)

# ---- Test 2: compare with boxplot.stats() -----------------------------------
boxplot.stats(sample_data)$out                           # also 999
boxplot.stats(sample_data)$stats[c(2, 4)]                # hinges 15 and 25

# ---- Test 3: the vector where v1/v2 disagreed with boxplot.stats() ----------
tricky <- c(64, 57, 53, 47, 55, 45, 47, 61, 54, 52, 25, 59, 63, 41, 17,
            50, 44, 40, 50, 57, 65, 31)
setdiff(tricky, remove_outliers_v1(tricky))              # v1 removes 25 and 17
remove_outliers_v2(tricky)$outliers                      # v2 also 25 and 17
remove_outliers(tricky)$outliers                         # v3: 17 only
boxplot.stats(tricky)$out                                # boxplot.stats: 17
stopifnot(identical(remove_outliers(tricky)$outliers, boxplot.stats(tricky)$out))

# ---- Test 4: 1,000 random synthetic vectors vs boxplot.stats() --------------
set.seed(400)
matches <- vapply(1:1000, function(i) {
  n <- sample(5:200, 1)                                  # random length
  y <- round(rnorm(n, mean = 50, sd = 10), 1)            # normal data
  y[sample(n, 3)] <- y[sample(n, 3)] + sample(c(-60, 60), 3, replace = TRUE)
  identical(sort(remove_outliers(y)$outliers), sort(boxplot.stats(y)$out))
}, logical(1))
cat("v3 agrees with boxplot.stats() on", sum(matches), "of 1000 vectors\n")
stopifnot(all(matches))

# ---- Test 5: synthetic data frame (grouped use) -----------------------------
set.seed(7)
sales <- data.frame(
  region = rep(c("East", "West"), each = 50),
  amount = c(rnorm(50, 200, 20), rnorm(50, 300, 30))
)
sales$amount[c(5, 60)] <- c(900, -50)                    # plant two outliers
lapply(split(sales$amount, sales$region),
       function(v) round(remove_outliers(v)$outliers, 2))

# ---- Test 6: edge cases (prompt P7) ------------------------------------------
remove_outliers(c(10, 15, NA, 999, 20, 25))              # NA dropped, n_na = 1
remove_outliers(c(5, 5, 5, 5))                           # IQR = 0, nothing removed
remove_outliers(c(5, 5, 5, 50))                          # IQR = 0, 50 removed
remove_outliers(42)                                      # single value
remove_outliers(c(1, 2, 3, 4, Inf))                      # Inf flagged
remove_outliers(sample_data, k = 3)$outliers             # extreme rule
remove_outliers(tricky, method = "quantile")$outliers    # type-7 option

# Errors that should be caught with clear messages
try(remove_outliers(c("a", "b")))                        # not numeric
try(remove_outliers(c(NA_real_, NA_real_)))              # no data
try(remove_outliers(c(1, NA, 3), na.rm = FALSE))         # NA not allowed
try(remove_outliers(sample_data, k = -1))                # bad k
try(remove_outliers(sample_data, method = "median"))     # bad method

cat("All tests passed.\n")
