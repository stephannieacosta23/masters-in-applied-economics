# Master's in Applied Economics

Applied time series work on the Dominican Republic, from my Master's in Applied Economics at UNIBE. The three projects ask related questions: what drives inflation, how do external shocks reach it, and does the answer change with the economic regime? All code is in R, written mostly in R Markdown.

| Project | Question | Methods |
|---|---|---|
| [01 · Dominican inflation by component](01-dominican-inflation-arima) | Why is headline inflation noisy while core inflation is slow and persistent? | ARIMA, unit root tests, residual diagnostics |
| [02 · Oil shocks and inflation](02-oil-shocks-inflation-svar) | How does an oil price shock pass through to prices in a small, import-dependent economy? | VAR, Cholesky and AB-type SVAR, sign restrictions, Blanchard-Quah |
| [03 · Crisis regimes and nonlinear dynamics](03-regime-switching-nonlinear-var) | Do growth, inflation and the policy rate behave differently in crises? | SETAR, LSTAR, threshold VAR, Markov-switching |

Each folder has its own README with the main results and the data needed to reproduce them. Scripts use relative paths, so open each project from its own folder.
