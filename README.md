# Master's in Applied Economics

Assignments, research projects and data analysis developed during my Master's in Applied Economics at UNIBE. Everything is written in R, mostly with R Markdown, using Dominican Republic macroeconomic data.

## Projects

| Folder | Topic | Methods |
|---|---|---|
| [`tarea-1-arima-ipc`](tarea-1-arima-ipc) | Dynamics and persistence of Dominican inflation by CPI component | ARIMA, unit root tests, residual diagnostics |
| [`tarea-2-svar`](tarea-2-svar) | Transmission of oil price shocks to Dominican inflation | VAR, recursive (Cholesky) and AB-type SVAR, sign restrictions, Blanchard-Quah |
| [`tarea-3-nonlinear-time-series`](tarea-3-nonlinear-time-series) | Regime-dependent transmission of shocks in GDP growth, inflation and the policy rate | SETAR, LSTAR, TVAR, Markov-switching |

## Reproducing

Each folder has its own README with the data it needs. The scripts read their data with relative paths, so open each project from its own folder. Packages are installed automatically by the scripts when missing.
