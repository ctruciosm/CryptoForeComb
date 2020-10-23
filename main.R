###########################################
### Forecasting Combination VaR and ES  ###
###########################################
#### Main Code
#### Change de getwd() in ForeComb and Comparison and Figures
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/")
library(rugarch)
library(GAS)
library(esback)
source("Optimizations.R")
source("esr_backtest_modified.R")
source("Function_VaR_VQR.R")
source("outliersviawavelets.R")
source("ForeComb.R")
source("ForeComb_wav.R")
source("Comparison.R")



#library(segMGarch)
