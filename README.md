# Master's in Applied Economics

Applied econometrics on the Dominican Republic, from my Master's in Applied Economics at UNIBE. The first three projects are time series work on inflation: what drives it, how external shocks reach it, and whether the answer changes with the economic regime. The fourth is my thesis, on returns to education in a segmented labor market, using ENCFT microdata. All code is in R, written mostly in R Markdown.

| Project | Question | Methods |
|---|---|---|
| [01 · Dominican inflation by component](01-dominican-inflation-arima) | Why is headline inflation noisy while core inflation is slow and persistent? | ARIMA, unit root tests, residual diagnostics |
| [02 · Oil shocks and inflation](02-oil-shocks-inflation-svar) | How does an oil price shock pass through to prices in a small, import-dependent economy? | VAR, Cholesky and AB-type SVAR, sign restrictions, Blanchard-Quah |
| [03 · Crisis regimes and nonlinear dynamics](03-regime-switching-nonlinear-var) | Do growth, inflation and the policy rate behave differently in crises? | SETAR, LSTAR, threshold VAR, Markov-switching |
| [04 · Thesis: returns to education](04-returns-to-education-thesis) | What is a year of schooling worth when the labor market is split between formal and informal jobs? | Mincer, Heckman, switching regression, Lewbel IV, cohort pseudopanel |

Each folder has its own README with the main results and the data needed to reproduce them. Scripts use relative paths, so open each project from its own folder.
