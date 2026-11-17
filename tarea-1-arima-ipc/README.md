# Univariate time series analysis of Dominican inflation

Univariate analysis of the main components of the Dominican Consumer Price Index (CPI): total, core, food and non-alcoholic beverages, and transport. The sample covers monthly data from January 2006 to December 2024, which includes the 2008–2009 financial crisis, COVID-19 and the recent supply-chain and energy shocks.

ARIMA models are fitted to each component to describe temporal dependence, inflation persistence and volatility.

## Files

- `tarea-1-arima-ipc.Rmd`: code and analysis (in Spanish). Knits to PDF.
- `tarea-1-arima-ipc.pdf`: rendered report.
- `data/base_tarea_1.xlsx`: monthly CPI series used in the analysis (sheet `DATA`), with the source tables on the other sheets.

## Run

Open the `.Rmd` with this folder as the working directory and knit it. R packages: readxl, tidyverse, forecast, tseries, urca, gridExtra, zoo, lubridate, knitr, kableExtra, moments.
