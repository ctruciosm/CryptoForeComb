###########################################
### Forecasting Combination VaR and ES  ###
###########################################
#### Main Code
#### Change de getwd() in ForeComb and Comparison and Figures
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/")
source("Optimizations.R")
source("esr_backtest2.R")
source("Function_VaR_VQR.R")
source("ForeComb.R")
source("Comparison7.R")



library(segMGarch)
TL(y=ret,VaR=VaR_AL[,3],VaR_level = 0.95)


setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/BTC/")

VaR_AL = read.csv("VaR_AL.csv",sep = " ")[-c(1:7),]
VaR_NZ = read.csv("VaR_NZ.csv",sep = " ")[-c(1:7),]
VaR_FZG = read.csv("VaR_FZG.csv",sep = " ")[-c(1:7),]

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/BTC/")
ret = read.csv("VaR.csv")[-c(1:7),"OoS"]