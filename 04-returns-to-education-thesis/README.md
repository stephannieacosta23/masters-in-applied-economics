# The price of studying in a segmented market: returns to education, informality and the gender gap in the Dominican Republic, 2016–2024

Master's thesis (UNIBE, April 2026; advisor: Pablo Flores). How much is one more year of schooling worth in a labor market where more than half of workers are informal, and how much of the gender wage gap is selection into work rather than unequal pay? The data are the stacked ENCFT microdata from 2016 to 2024, restricted to 285,515 employed people aged 16 to 65 who report income and hours.

## Models

The extended OLS is the main model. The others answer one identification problem each:

1. Base OLS: the standard Mincer equation, used to measure omitted variable bias.
2. Extended OLS: Mincer plus sex, informality and 17 ISIC Rev. 4 sector dummies, with HC1 errors.
3. Heckman two-step: corrects selection into employment, using household headship, marital status and remittances as exclusion restrictions.
4. Switching regression: separate wage equations for the formal and informal regimes, since informality is endogenous.
5. Lewbel (2012) 2SLS: internal instruments from heteroskedasticity for the endogeneity of schooling.
6. Cohort pseudopanel with fixed effects and the Deaton–Paxson normalization, to check that the return is not a generational composition effect.

## Main results

- Each extra year of schooling raises monthly income by 3.82%, against a Latin American average of about 11%. Leaving out informality and sector pushes the estimate up to 4.95%.
- Workers in the informal sector earn 23.7% less than comparable formal workers.
- The return to schooling is 6.26% in the formal sector and 2.37% in the informal sector.
- The income–hours elasticity is 0.54, so income per hour falls as the work week gets longer.
- The gender gap goes from −22.3% in OLS to −4.1% after the Heckman correction. Most of the raw gap comes from who participates in the labor market.
- With Lewbel instruments the return drops to 2.11%. The Sargan test rejects, so this is better read as a bound than as a clean causal estimate.

## Files

- `returns-to-education-thesis.pdf`: the full thesis (Spanish).
- `returns-to-education-analysis.html`: rendered R Markdown with code, tables, figures and robustness checks (Spanish).
- `returns-to-education-encft.R`: the analysis code as a plain R script, taken from the R Markdown chunks.
- `data/`: empty. The ENCFT microdata (702,208 rows × 391 columns) are too large for GitHub and are published by the Banco Central de la República Dominicana. Save the stacked 2016–2024 file as `data/encft_2016_2024.xlsx`, or change `params$ruta_datos` at the top of the script.

## Run

Open the script with this folder as the working directory. R packages: readxl, tidyverse, skimr, lmtest, sandwich, censReg, plm, sampleSelection, car, kableExtra, corrplot, GGally, reshape2, RColorBrewer, gridExtra, quantreg, purrr, AER. The full dataset needs a good amount of RAM.
