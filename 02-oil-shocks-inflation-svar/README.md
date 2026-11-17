# Oil shocks and inflation in a small open economy: a SVAR for the Dominican Republic

How does a rise in the oil price reach Dominican prices and activity? A six-variable monthly system, February 2006 to December 2024: US industrial production, WTI oil price, the Dominican activity index (IMAE), CPI, the interbank rate and the exchange rate. All variables are in log differences except the rate, which is in first differences.

## Identification strategies compared

The same question is answered with four different sets of assumptions, so the results can be checked against each other:

1. **Recursive (Cholesky)** under two variable orderings, with impulse responses and variance decomposition of inflation.
2. **AB-type structural VAR**, with economic restrictions on contemporaneous effects: external variables are exogenous, and the interest rate reacts to inflation like a monetary rule.
3. **Sign restrictions**: an oil price shock must raise oil prices and inflation and lower domestic activity over the first six months. Rotations are drawn at random and only those satisfying the signs are kept.
4. **Blanchard-Quah** on a bivariate output-inflation system, separating supply from demand shocks through long-run restrictions.

## What the diagnostics show

- BIC selects a single lag and the system is stable (all roots inside the unit circle).
- Residuals show no serial correlation (Portmanteau p = 0.72), but strong ARCH effects and non-normality. They come from the 2014–2016 oil price collapse and the COVID-19 period, which fits an economy exposed to large external shocks.
- WTI is by far the most volatile series (monthly standard deviation of 10.9%, with a 59% drop in April 2020). Domestic prices and the exchange rate are much calmer, which supports ordering the external variables first.

## Files

- `oil-shocks-inflation-svar.R`: full script, with the economic interpretation written in the comments (Spanish).
- `data/macro_monthly.xlsx`: monthly dataset (sheet `DATA`).

## Run

Open the script with this folder as the working directory. R packages: readxl, tidyverse, tseries, zoo, seasonal, vars, svars, ggplot2, gridExtra, kableExtra, urca.
