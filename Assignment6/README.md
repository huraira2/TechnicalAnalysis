# Assignment 6 – Technical Analysis using R, Visualization Phase

**Nadeeka Godage (664430944) – BDA400**

An interactive **R Shiny portfolio dashboard**. It fetches stock prices from Yahoo Finance and shows them as candlestick, line, area or OHLC-bar charts. Technical indicators can be overlaid and switched on or off, and the bars where a chosen trading rule says Buy or Sell are annotated on the chart.

It builds on the earlier stages:
- **Assignment 2:** the portfolio (`../Assignment2/portfolio.txt`: AAPL, MSFT, GOOGL, AMZN, TSLA) and the date range.
- **Assignment 5:** the base-R indicators (`sma`, `ema`, `macd`, `rsi`, `stdev`, `linreg`, `crossover`, `crossunder`), which the app loads from `../Assignment5`.

## Files

| File | Description |
|------|-------------|
| `app.R` | The complete dashboard, in sections that follow the brief: Step 1 data, Step 2 visualization, Step 3 indicators, Step 4 trading rules, then UI and server |
| `test_app.R` / `test_app_log.txt` | Server tests with `shiny::testServer()` (18 checks: every chart type, time frame, rule and error case) |
| `screenshots/` | Screenshots of the running app |
| `NadeekaGodage_BDA400_A06.docx` / `.pdf` | Cover page, project description and walkthrough |
| `NadeekaGodage_BDA400_A06.zip` | Everything above in one self-contained folder, including copies of the Assignment 5 indicators, the offline prices and the portfolio, so the app runs on its own |

## How to run

```r
install.packages(c("shiny", "bslib", "ggplot2", "quantmod", "patchwork", "scales"))
shiny::runApp("Assignment6")   # run from the repository folder
```

You can also open `app.R` in RStudio and click **Run App**. In the repository, the app reads `../Assignment2` (portfolio) and `../Assignment5` (indicators and offline prices).

**From the zip (`NadeekaGodage_BDA400_A06.zip`):** the zip is self-contained. Copies of the Assignment 5 indicators and the offline prices are in `indicators/`, and the portfolio is in `portfolio.txt`. Unzip it, open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("NadeekaGodage_BDA400_A06")` from the folder that contains it.

## Features

- **Data:**
  - Yahoo Finance through `quantmod::getSymbols()`, for any ticker you type.
  - If Yahoo fails, the app uses the saved Assignment 5 CSV automatically and shows a notice.
  - Missing prices are filled.
  - Bad tickers and date ranges give clear messages instead of crashing.
- **Widgets:** stock, data source, date range, time frame (Daily / Weekly / Monthly), and chart type (Candlestick / Line / Area / OHLC Bars).
- **Indicators, each switched on or off:**
  - layers on the price chart: moving averages (SMA or EMA, with adjustable periods), Bollinger Bands, and a linear-regression trend line.
  - panels below the chart on the same date axis: Volume, RSI, MACD.
- **Trading rules, with adjustable settings:**
  - MA crossover (the brief's rule).
  - RSI overbought/oversold.
  - MACD signal-line crossover.
  - MA crossover with an RSI filter.
- **Annotations:**
  - Buy/Sell labels and arrows on the trade bars.
  - optional shading of the Buy and Sell zones.
  - optional Buy/Sell/Hold label on every bar, as in the brief's `geom_text` example.
- **Results:**
  - value boxes showing the last close, the current signal, the strategy return and the buy-and-hold return.
  - a Signals table.
  - a Portfolio tab comparing all five stocks.
  - a Data tab showing the source and how much missing data was filled.

The strategy return is a simple illustration with no trading costs. It is not investment advice.
