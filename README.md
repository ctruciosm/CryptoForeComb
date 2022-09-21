# CryptoForeComb

Codes used in the paper "A Comparison of Methods for Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies" of Trucíos and Taylor, 2022.

## Instructions

Daily BTC and ETH prices were downloaded from Binance and are in the Data folder. The codes used are described below:


### Main functions:

- `VaR_ES.R` : Estimates the VaR and ES using the methods described in the paper. crytocurrency = "BTC/" performs the results for BTC, crytocurrency = "ETH/" performs the results for ETH.
- `ForeComb.R`: Performs the combining methods proposed by Taylor (2020) and briefly described in Section 3.2 (the combining methods described in Section 3.1 are included in the `Comparison.R` file).
- `Comparison.R`: This code performs the backtesting exercise for the VaR and ES, the final output are the Tables reported in the paper (in .tex format). Additionaly, the model confidence set and the fluctiation test are performed.


### Auxiliary functions:

- `Optimizations.R`: Used in `ForeComb.R` and `Comparison.R` codes. Optimizations.R, implements the scoring functions used and the optimization weights routines.
- `scoring_functions.cpp`: Implements the scoring functions in C++.
- `Function_VaR_VQR.R`: Implements the VQR backtesting of Gaglianone et al. (2011).
- `CAViaR.R` and `CAViaR.cpp`: implements the CAViaR, CAViaREVT and CAViaRALD methods.
- `DescriptiveStatistics.R`: Table 1 and Figure 1 of the paper.
- `GiacominiRossiTest.R`: Fluctuation test of Giacomini and Rossi (2010)

The routines should be performed in the following ordering:

1. `VaR_ES.R`
2. `ForeComb.R`
3. `Comparison.R`


## References

- Taylor, J. W. (2020). Forecast combinations for value at risk and expected shortfall. International Journal of Forecasting, 36(2), 428-441.
- Trucíos, C. and Taylor, J. W. (2022). A Comparison of Methods for Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies. Working Paper (Submitted)
- Gaglianone, W. P., Lima, L. R., Linton, O., & Smith, D. R. (2011). Evaluating value-at-risk models via quantile regression. Journal of Business & Economic Statistics, 29(1), 150-160.
- Giacomini, R. and Rossi, B. (2010). Forecast comparisons in unstable environments. Journal of Applied
Econometrics, 25(4):595-620.
