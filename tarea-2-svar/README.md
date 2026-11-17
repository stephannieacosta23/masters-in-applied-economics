# Oil price shocks and Dominican inflation: SVAR models

Monthly six-variable VAR (February 2006 to December 2024) with US industrial production, WTI oil price, the Dominican activity index (IMAE), CPI, the interbank interest rate and the exchange rate, all in log differences except the rate.

The script covers:

1. Data preparation, seasonality tests and descriptive statistics.
2. VAR estimation, lag selection and diagnostics (autocorrelation, ARCH, normality, stability).
3. Recursive identification (Cholesky) under two variable orderings: impulse responses and variance decomposition of inflation.
4. Structural VAR with contemporaneous (AB) restrictions.
5. Sign restrictions for an oil price shock (oil price up, inflation up, activity down).
6. Bivariate Blanchard-Quah model of output growth and inflation.

## Files

- `tarea-2-svar.R`: full script, with economic interpretation embedded in the comments.
- `data/base_tarea_2.xlsx`: monthly dataset (sheet `DATA`).

## Run

Open `tarea-2-svar.R` with this folder as the working directory. R packages: readxl, tidyverse, tseries, zoo, seasonal, vars, svars, ggplot2, gridExtra, kableExtra, urca.
