# EWZ/IBOV Volatility Analysis

A minimalistic R pipeline for financial time-series analysis, focusing on volatility regimes and Granger-causality between the Bovespa Index (**IBOV**) and the iShares MSCI Brazil ETF (**EWZ**).

---

## Pipeline Overview

The analysis follows a rigorous econometric workflow designed for financial data:

1.  **Data Acquisition**: Automated fetching of daily close prices from Yahoo Finance via `yfR`.
2.  **Stationarity Diagnostics**: Augmented Dickey-Fuller (ADF) tests, supplemented by Ljung-Box and ARCH-LM tests for residual diagnostics.
3.  **Regime-Switching GARCH**: Modeling volatility using **Markov-Switching GARCH** (2 regimes) with `eGARCH` variance specifications to capture asymmetric shocks.
4.  **Granger Causality**: Robust Wald tests (HC1) to identify lead-lag relationships between the estimated volatility series.
5.  **Visualization**: Thematic time-series plots with automated event highlighting (e.g., COVID-19, geopolitical shifts).

## Project Structure

```text
.
├── run_analysis.R      # Main entry point
├── setup/              # Data fetching and environment setup
├── analysis/           # Econometric modeling (ADF, GARCH, Granger)
├── visualization/      # High-quality ggplot2 implementations
└── outputs/            # Generated PDF reports and plots
```

## Usage

Ensure you have the required libraries installed via `setup/imports.R`, then execute the main script:

```r
source("run_analysis.R")
```

## Core Dependencies

*   `MSGARCH` & `rugarch`: Advanced volatility modeling.
*   `yfR`: Yahoo Finance data interface.
*   `urca` & `tseries`: Unit root and stationarity testing.
*   `ggplot2` & `patchwork`: Publication-ready visualizations.
