# Dominican inflation by component: why headline CPI is noisy and core CPI is not

Four components of the Dominican Consumer Price Index (total, core, food and non-alcoholic beverages, transport), monthly from January 2006 to December 2024. The period includes the 2008–2009 crisis, COVID-19 and the 2021–2022 inflation surge. One ARIMA model per component, to compare how persistent and how volatile each one is.

## What I found

| Component | Model | What the dynamics say |
|---|---|---|
| Total | ARIMA(1,1,1) | Moderate persistence (AR = 0.43). Residuals are clean (Ljung-Box p = 0.41). |
| Core | ARIMA(3,1,1) | Slow, gradual adjustment spread over about three months, typical of sticky prices. The most persistent component, but residual autocorrelation remains, so I treat the fit with reservations. |
| Food | ARIMA(1,1,1) | Transitory shocks dominate (MA = 0.55) and fade fast. Highest average monthly inflation (0.47%). |
| Transport | ARIMA(1,1,0) | The simplest model for the most volatile series: residual variance is about 66 times that of core inflation. Tied to oil, since the country imports most of its fuel. |

- All four price levels are I(1): shocks to the level are permanent, while inflation rates are stationary.
- Short-run swings in headline inflation come mostly from the volatile components. Core inflation is the better signal of medium-term price pressure.
- Limits I flag in the report: no seasonal terms, no explicit treatment of structural breaks such as COVID-19, and constant variance assumed. GARCH, ARIMAX and regime-switching models are the natural next steps, and project 03 takes one of them.

## Files

- `dominican-inflation-arima.Rmd`: code and analysis, in Spanish. Knits to PDF.
- `dominican-inflation-arima.pdf`: rendered report.
- `data/cpi_components_monthly.xlsx`: monthly series (sheet `DATA`), with the source tables on the other sheets.

## Run

Open the `.Rmd` with this folder as the working directory and knit it. R packages: readxl, tidyverse, forecast, tseries, urca, gridExtra, zoo, lubridate, knitr, kableExtra, moments.
