# CryptoForeComb

Codes used in the paper "Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies using Combinations based on Models with Jumps and Regime-Switching" of Trucíos and Taylor, 2020.

## Instructions

The BTC, ETH, LTC and XRP data downloaded from Binance are in the Data folder. The codes used are described below:

- VaR_ES.R : Estimates the VaR and ES using the three individual methods described in the paper. crytocurrency = "BTC/" perform the results for BTC, other options are "ETH/", "LTC/" and "XRP/".
- ForeComb.R: Performs the combining methods described in Taylor, 2020.
- Comparison.R: This code performs the backtesting exercise for the VaR and ES, the final output are the Tables reported in the paper (in .tex format). The MCS and the scoring functions without March 12, 2020 can be also obtaned slightly modifying the code.
- Optimizations.R: Auxiliary functions used in ForeComb and Comparison codes. Optimizations.R, implements the scoring functions used and the optimization weights routines.
- esr_backtest_modified.R: A slight modification of the esr_backtest function provided in the esback R package (Bayer and Dimitriadis, 2020). Basically, we add nlminb is Nelder-Mead doesn't work and the the loss function used, we exclude NA if any (see the conditional_mean_sigma_modified function inside).
- Function_VaR_VQR.R: Implements the VQR backtesting of Gaglianone et al. (2011).
- VaR_ES_GARCH_Type.R: Estimates the VaR using GARCH-Type models (in case you are interested in those results)
- DescriptiveStatistics: Table 1 and Figure 1 of the paper.

How to use? VaR_ES.R >>> ForeComb.R >>> Comparison.R




## References

- Taylor, J. W. (2020). Forecast combinations for value at risk and expected shortfall. International Journal of Forecasting, 36(2), 428-441.
- Trucíos, C. and Taylor, J. W. (2020). Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies using Combinations based on Models with Jumps and Regime-Switching. Manuscript
- Bayer, S., & Dimitriadis, T. (2010). esback: Expected Shortfall Backtesting. R package version 0.3.0
- Gaglianone, W. P., Lima, L. R., Linton, O., & Smith, D. R. (2011). Evaluating value-at-risk models via quantile regression. Journal of Business & Economic Statistics, 29(1), 150-160.
