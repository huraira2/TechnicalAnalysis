# TechnicalAnalysis

Stock-market technical analysis in R for **BDA400 – Data Science Tools and Techniques**.
This is a three-stage project, and each assignment is kept in its own folder.

**Author:** Nadeeka (Student ID: 664430944)

| Folder | Assignment | Status |
|--------|------------|--------|
| [`Assignment2/`](Assignment2) | Assignment 2 – Technical Analysis using R, Preliminary Stage | Complete |
| [`Assignment3/`](Assignment3) | Assignment 3 – Prompting R Functions with AI (outlier-removal function, tests, reflection) | Complete |
| [`Assignment4/`](Assignment4) | Assignment 4 – Ask AI: Clean This Dataset with R and dplyr | In progress (reflection to add) |

## Assignment 2 contents

| File | Description |
|------|-------------|
| `Assignment2/portfolio.txt` | Portfolio stock symbols, one per line (AAPL, MSFT, GOOGL, AMZN, TSLA) |
| `Assignment2/TechnicalAnalysis_A01.Rmd` | R Markdown with `load_stock_data()`, `calculate_statistics()`, display functions and charts |
| `Assignment2/TechnicalAnalysis_A01.html` | Knitted output of the R Markdown file |
| `Assignment2/Nadeeka_BDA400_A02_Report.docx` | Step-by-step report with screenshots |
| `Assignment2/Nadeeka_BDA400_A02.docx` | Cover page with student details and repository link |

## How to run

```r
install.packages(c("quantmod", "TTR"))
```

Open `Assignment2/TechnicalAnalysis_A01.Rmd` in RStudio (keep `portfolio.txt` in the same folder) and click **Knit**.

## Data source

Daily OHLCV prices from Yahoo Finance via `quantmod::getSymbols()`. Data is for educational use only and is not investment advice.
