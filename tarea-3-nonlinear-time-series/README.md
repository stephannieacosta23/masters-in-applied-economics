# Nonlinear models in the mean: the Dominican case

Quarterly application with real GDP growth, CPI inflation and the monetary policy rate of the Dominican Republic (2007-Q2 to 2024-Q3, 70 observations).

The report tests for stationarity, lag selection and nonlinearity, then compares a linear VAR against SETAR, LSTAR, threshold VAR (with impulse responses built from simulation) and Markov-switching models. It closes with a case study of the 2021–2023 inflationary shock.

## Files

- `tarea-3-nonlinear-time-series.Rmd`: code and analysis (in Spanish). Knits to HTML.

## Data

The raw files are manual downloads from the Banco Central de la República Dominicana (BCRD) and are not included in this repository. To reproduce, place these files in `data/raw/`:

- `pib_real_trimestral.xlsx`: quarterly real GDP index
- `ipc_mensual.xls`: monthly CPI
- `tpm_mensual.xlsx`: monthly monetary policy rate

The script creates `data/processed/` for the intermediate file.

## Run

Open the `.Rmd` with this folder as the working directory and knit it. The script installs missing packages.
