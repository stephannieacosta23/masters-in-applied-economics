# Crisis regimes and nonlinear dynamics: growth, inflation and the policy rate in the Dominican Republic

Do the economy and monetary policy behave the same way in calm times and in crises? Quarterly data on real GDP growth, CPI inflation and the monetary policy rate, 2007-Q2 to 2024-Q3 (70 observations). A linear VAR is the benchmark, then compared against SETAR, LSTAR, threshold VAR and Markov-switching models.

## What I found

- **The linear model is not enough.** The BDS test rejects linearity for growth and inflation (p < 0.001) and the Teräsvirta test rejects it for the policy rate (p = 0.043). The linear VAR also fails normality and homoskedasticity, so it works only as a descriptive reference.
- **There is a crisis regime.** The threshold models (SETAR, TVAR) isolate a regime covering about 10% of the sample, matching 2008–2009 and 2020. Autoregressive coefficients differ sharply between regimes, so persistence and mean reversion depend on the state of the economy.
- **Markov-switching finds the same episodes without a threshold variable.** It is most useful for inflation, where parametric tests do not reject linearity but BDS does, which points to discrete regime changes rather than smooth transitions.
- **Impulse responses are asymmetric.** Regime-dependent responses (a threshold VAR built by simulation, and Markov-switching conditional responses) show asymmetries that the linear VAR hides by construction. In the 2021–2023 inflation shock this matters: the central bank reacted more aggressively than the linear model predicts.

## Files

- `regime-switching-nonlinear-var.Rmd`: code and analysis, in Spanish. Knits to HTML.

## Data

The raw files are manual downloads from the Banco Central de la República Dominicana (BCRD) and are not included. To reproduce, place them in `data/raw/`:

- `pib_real_trimestral.xlsx`: quarterly real GDP index
- `ipc_mensual.xls`: monthly CPI
- `tpm_mensual.xlsx`: monthly monetary policy rate

The script creates `data/processed/` for the intermediate file.

## Run

Open the `.Rmd` with this folder as the working directory and knit it. The script installs missing packages.
