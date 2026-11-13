# Assignment 5 – Technical Analysis using R, Development Phase

**Nadeeka Godage (664430944) – BDA400**

Nine technical indicators written in **base R only**, one file each, using the function names from the assignment templates.

| File | Function |
|------|----------|
| `sma.R` | `sma(data, period)` – Simple Moving Average |
| `ema.R` | `ema(data, period)` – Exponential Moving Average |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` – uses `ema.R` |
| `stdev.R` | `stdev(data)` – population standard deviation |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` |
| `rsi.R` | `rsi(data, period)` – Wilder RSI |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` – uses `rsi.R`, `sma.R` |
| `crossover.R` | `crossover(arr1, arr2)` – "Up" / "Down" / "None" |
| `crossunder.R` | `crossunder(arr1, arr2)` – "True" / "False" |

Supporting files:

- `test_indicators.R` – 46 checks: the brief's examples, hand-worked answers, error messages, and comparisons with `sd()`, `lm()`, `stats::filter()` and TTR (TTR is used for checking only). Output: `test_output_log.txt`.
- `stock_demo.R` – applies every indicator to the Assignment 2 portfolio (AAPL, MSFT, GOOGL, AMZN, TSLA, 24 Aug 2025 – 24 Aug 2026). Output: `stock_demo_log.txt`, `indicator_summary.csv`, `charts/`.
- `get_stock_data.R` – downloads the prices to `stock_prices.csv` (needs quantmod; the CSV is already included).
- `NadeekaGodage_BDA400_A05.docx` / `.pdf` – cover page and an explanation of each implementation.

## How to run

Set the working directory to this folder (RStudio: *Session > Set Working Directory > Choose Directory*), then:

```r
source("sma.R"); sma(c(10, 12, 15, 20, 18, 22, 25, 24, 21), 3)   # any single indicator
source("test_indicators.R")                                      # should end: 46 passed, 0 failed
source("stock_demo.R")                                           # portfolio summary and charts
```

Or from a terminal in this folder: `Rscript test_indicators.R` and `Rscript stock_demo.R`.
