# CryptoForeComb

Codes used in the paper "A Comparison of Methods for Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies" of Trucíos and Taylor, 2022.

## Instructions

Daily BTC and ETH prices were downloaded from Binance and are in the Data folder. The codes used are described below:


### Main functions:

- VaR_ES.R : Estimates the VaR and ES using the methods described in the paper. crytocurrency = "BTC/" performs the results for BTC, crytocurrency = "ETH/" performs the results for ETH.
- ForeComb.R: Performs the combining methods described in Taylor, 2020.
- Comparison.R: This code performs the backtesting exercise for the VaR and ES, the final output are the Tables reported in the paper (in .tex format). 


### Auxiliary functions:

- Optimizations.R: Used in ForeComb and Comparison codes. Optimizations.R, implements the scoring functions used and the optimization weights routines.
- scoring_functions.cpp: Implements the scoring functions in C++.
- Function_VaR_VQR.R: Implements the VQR backtesting of Gaglianone et al. (2011).
- CAViaR.R and CAViaR.cpp: implements the CAViaR, CAViaREVT and CAViaRALD methods.
- DescriptiveStatistics.R: Table 1 and Figure 1 of the paper.
- GiacominiRossiTest.R: Fluctuation test of Giacomini and Rossi (2010)

> How to use? VaR_ES.R >>> ForeComb.R >>> Comparison.R


## References

- Taylor, J. W. (2020). Forecast combinations for value at risk and expected shortfall. International Journal of Forecasting, 36(2), 428-441.
- Trucíos, C. and Taylor, J. W. (2022). A Comparison of Methods for Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies. Working Paper (Submitted)
- Bayer, S., & Dimitriadis, T. (2010). esback: Expected Shortfall Backtesting. R package version 0.3.0
- Gaglianone, W. P., Lima, L. R., Linton, O., & Smith, D. R. (2011). Evaluating value-at-risk models via quantile regression. Journal of Business & Economic Statistics, 29(1), 150-160.
- Giacomini, R. and Rossi, B. (2010). Forecast comparisons in unstable environments. Journal of Applied
Econometrics, 25(4):595-620.
