# =============================================================================
# test_indicators.R - checks every indicator function
# BDA400 Assignment 5 | Nadeeka Godage (664430944)
#
# Run from the Assignment5 folder:   Rscript test_indicators.R
#   (or in RStudio: Session > Set Working Directory > To Source File Location,
#    then click Source)
#
# 1. Runs the example inputs from the assignment brief.
# 2. Tests the error messages and edge cases.
# 3. Compares results with trusted versions (base R sd()/lm(), and TTR's
#    SMA/RSI if the TTR package is installed). TTR is used ONLY here to check
#    the answers - the indicator files themselves use base R only.
# =============================================================================

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
            "stoch_rsi.R", "crossover.R", "crossunder.R")) {
  source(f)
}

passed <- 0
failed <- 0
check <- function(name, ok) {
  if (isTRUE(ok)) {
    passed <<- passed + 1
    cat(sprintf("  PASS  %s\n", name))
  } else {
    failed <<- failed + 1
    cat(sprintf("  FAIL  %s\n", name))
  }
}
gives_error <- function(expr, message) {
  result <- tryCatch({ expr; "no error" }, error = function(e) conditionMessage(e))
  identical(result, message)
}
near <- function(a, b, tol = 1e-8) {
  length(a) == length(b) && all(is.na(a) == is.na(b)) &&
    all(abs(a[!is.na(a)] - b[!is.na(b)]) < tol)
}

prices <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# ---------------------------------------------------------------- 1. examples
cat("\n== 1. Examples from the assignment brief ==\n")

cat("\nsma(c(10,12,15,20,18,22,25,24,21), 3):\n"); print(sma(prices, 3))
cat("\nema(c(10,12,15,20,18,22,25,24,21), 3):\n"); print(ema(prices, 3))
cat("\nmacd(c(100,105,110,115,120,125,130), 3, 5, 2):\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2))
cat("\nstdev(c(10,12,15,20,18,22,25,24,21)):\n"); print(stdev(prices))
cat("\nlinreg(c(10,12,15,20,18,22,25,24,21), 5, 0):\n"); print(linreg(prices, 5, 0))
rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
cat("\nrsi(c(45,50,48,55,52,49,58,60,65,62), 5):\n"); print(rsi(rsi_data, 5))
cat("\nstoch_rsi(same data, 14, 3, 3)  - 10 prices are not enough for period 14:\n")
print(stoch_rsi(rsi_data, 14, 3, 3))
cat("\nstoch_rsi(same data, 3, 3, 3):\n"); print(stoch_rsi(rsi_data, 3, 3, 3))
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cat("\ncrossover(arr1, arr2):\n");  print(crossover(arr1, arr2))
cat("\ncrossunder(arr1, arr2):\n"); print(crossunder(arr1, arr2))

# ------------------------------------------------------ 2. hand-worked answers
cat("\n== 2. Answers worked out by hand ==\n")
check("sma period 3 first value = (10+12+15)/3", near(sma(prices, 3)[1], 37 / 3))
check("sma returns n - period + 1 values", length(sma(prices, 3)) == 7)
check("sma period = length gives the overall mean", near(sma(prices, 9), mean(prices)))
check("sma period 1 returns the data unchanged", near(sma(prices, 1), prices))
check("ema first value = first price", ema(prices, 3)[1] == 10)
check("ema second value = (12-10)*0.5 + 10 = 11", near(ema(prices, 3)[2], 11))
check("ema same length as data", length(ema(prices, 3)) == length(prices))
check("ema of a constant series stays constant", near(ema(rep(5, 10), 4), rep(5, 10)))
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2)
check("macd histogram = macd_line - signal_line", near(m$histogram, m$macd_line - m$signal_line))
check("macd line = ema(3) - ema(5)",
      near(m$macd_line, ema(c(100, 105, 110, 115, 120, 125, 130), 3) -
                        ema(c(100, 105, 110, 115, 120, 125, 130), 5)))
check("macd list names", identical(names(m), c("macd_line", "signal_line", "histogram")))
check("stdev of c(2,4,4,4,5,5,7,9) = 2 (textbook example)", near(stdev(c(2, 4, 4, 4, 5, 5, 7, 9)), 2))
check("stdev of a constant series = 0", stdev(rep(3, 6)) == 0)
lr <- linreg(c(2, 4, 6, 8, 10), 5, 0)
check("linreg on a straight line: slope 2", near(lr$slope, 2))
check("linreg on a straight line: intercept 0", near(lr$intercept, 0))
check("linreg predicted values = the line", near(lr$predicted_values, c(2, 4, 6, 8, 10)))
check("linreg uses exactly the last 5 points",
      length(linreg(prices, 5, 0)$predicted_values) == 5)
check("linreg on the last 5 points (18,22,25,24,21): slope 0.8",
      near(linreg(prices, 5, 0)$slope, 0.8))
check("rsi first 'period' values are NA", all(is.na(rsi(rsi_data, 5)[1:5])))
check("rsi values between 0 and 100",
      all(rsi(rsi_data, 5)[6:10] >= 0 & rsi(rsi_data, 5)[6:10] <= 100))
check("rsi of a rising series = 100", all(rsi(1:20, 5)[6:20] == 100))
check("rsi of a falling series = 0", all(rsi(20:1, 5)[6:20] == 0))
check("crossover finds the Up cross at point 4", which(crossover(arr1, arr2) == "Up") == 4)
check("crossover finds a Down cross",
      identical(crossover(c(5, 5, 1), c(3, 3, 3)), c("None", "None", "Down")))
check("crossunder is True where arr1 drops below arr2",
      identical(crossunder(c(5, 5, 1), c(3, 3, 3)), c("None", "False", "True")))
check("crossunder example has no True (arr1 only crosses upward)",
      !any(crossunder(arr1, arr2) == "True"))
k <- stoch_rsi(rsi_data, 3, 3, 3)
check("stoch_rsi %K between 0 and 1", all(k$k_line[!is.na(k$k_line)] >= 0 & k$k_line[!is.na(k$k_line)] <= 1))
check("stoch_rsi %D = 3-point SMA of %K", near(k$d_line, sma(k$k_line, 3)))
check("stoch_rsi with too little data gives NA, not an error",
      all(is.na(stoch_rsi(rsi_data, 14, 3, 3)$k_line)))

# ---------------------------------------------------- errors and edge cases
cat("\n== 3. Error messages and edge cases ==\n")
check("sma: data shorter than period",
      gives_error(sma(1:3, 5), "Data length should be greater than or equal to the period"))
check("linreg: length too long",
      gives_error(linreg(1:5, 10, 0),
                  "regressionLength cannot be greater than the number of elements in regressionSource"))
check("linreg: offset >= length",
      gives_error(linreg(1:10, 5, 5), "regressionOffset must be less than regressionLength"))
check("crossover: different lengths",
      gives_error(crossover(1:3, 1:4), "Both arrays should have the same length"))
check("crossunder: different lengths",
      gives_error(crossunder(1:3, 1:4), "Both arrays should have the same length"))
check("crossover ignores NA points", crossover(c(NA, 5, 1), c(3, 3, 3))[2] == "None")
check("rsi with too little data warns and returns NA",
      all(is.na(suppressWarnings(rsi(1:5, 14)))))
check("rsi warning message is shown",
      tryCatch({ rsi(1:5, 14); FALSE }, warning = function(w) TRUE))

# ------------------------------------------------------ 4. trusted versions
cat("\n== 4. Compared with trusted R functions (random data, 500 prices) ==\n")
set.seed(400)
x <- cumsum(rnorm(500)) + 100

check("stdev = sd() * sqrt((n-1)/n)  (population vs sample)",
      near(stdev(x), sd(x) * sqrt((length(x) - 1) / length(x))))
y <- tail(x, 50)
fit <- lm(y ~ seq_along(y))
lr <- linreg(x, 50, 0)
check("linreg slope = lm() slope", near(lr$slope, unname(coef(fit)[2])))
check("linreg intercept = lm() intercept", near(lr$intercept, unname(coef(fit)[1])))
check("linreg predicted = lm() fitted values", near(lr$predicted_values, unname(fitted(fit))))
lr_off <- linreg(x, 50, 10)
check("linreg with offset 10 uses points 441 to 490",
      near(lr_off$slope, unname(coef(lm(x[441:490] ~ seq_len(50)))[2])))
check("sma = stats::filter moving average",
      near(sma(x, 20), as.numeric(stats::filter(x, rep(1 / 20, 20), sides = 1))[20:500]))
alpha <- 2 / (12 + 1)
check("ema = stats::filter recursive form",
      near(ema(x, 12), as.numeric(stats::filter(alpha * x, 1 - alpha,
                                                 method = "recursive", init = x[1]))))

if (requireNamespace("TTR", quietly = TRUE)) {
  check("sma = TTR::SMA (non-NA part)", near(sma(x, 20), as.numeric(na.omit(TTR::SMA(x, 20)))))
  check("rsi = TTR::RSI (Wilder, period 14)", near(rsi(x, 14), as.numeric(TTR::RSI(x, 14))))
} else {
  cat("  (TTR not installed - skipped the TTR comparisons)\n")
}

cat(sprintf("\n== RESULT: %d passed, %d failed ==\n", passed, failed))
if (failed > 0) quit(status = 1)
