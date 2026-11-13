# TechnicalAnalysis

Stock-market technical analysis in R for **BDA400 – Data Science Tools and Techniques**.
This is a three-stage project, and each assignment is kept in its own folder.

**Author:** Nadeeka (Student ID: 664430944)

| Folder | Assignment | Status |
|--------|------------|--------|
| [`A02/`](A02) | Assignment 2 – Technical Analysis using R, Preliminary Stage | Complete |
| `A03/` | Assignment 3 – *coming soon* | – |
| `A04/` | Assignment 4 – *coming soon* | – |

## Assignment 2 contents

| File | Description |
|------|-------------|
| `A02/portfolio.txt` | Portfolio stock symbols, one per line |
| `A02/technical_analysis_functions.R` | Functions: `read_portfolio()`, `load_stock_data()`, `calculate_statistics()`, display and plotting utilities |
| `A02/Nadeeka_BDA400_A02.Rmd` | R Markdown that imports, displays, summarises and visualises the data |
| `A02/Nadeeka_BDA400_A02.html` | Knitted output of the R Markdown file |
| `A02/Nadeeka_BDA400_A02_Report.docx` | Step-by-step report with screenshots |
| `A02/output/` | CSV exports of the downloaded prices and summary statistics |

## How to run

```r
install.packages(c("quantmod", "TTR", "xts", "zoo", "rmarkdown"))
setwd("A02")
source("technical_analysis_functions.R")
stocks <- load_stock_data("portfolio.txt")
stats  <- calculate_portfolio_statistics(stocks)
stats$summary
```

Or open `A02/Nadeeka_BDA400_A02.Rmd` in RStudio and click **Knit**.

## Data source

Daily OHLCV prices from Yahoo Finance via `quantmod::getSymbols()`. Data is for educational use only and is not investment advice.
