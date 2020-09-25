###########################################
### Forecasting Combination VaR and ES  ###
### Comparisons                         ###
###########################################

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/BTC/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/ETH/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/LTC/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/XRP/")



# Setting 
mu = read.csv("VaR.csv")[-c(1:7),"mu"]
VaR = as.matrix(read.csv("VaR.csv")[-c(1:7),-1]) + mu
ES = as.matrix(read.csv("ES.csv")[-c(1:7),-1]) + mu
ret = read.csv("VaR.csv")[-c(1:7),"OoS"]




VaR_AL = read.csv("VaR_AL.csv",sep = " ")[-c(1:7),]
VaR_NZ = read.csv("VaR_NZ.csv",sep = " ")[-c(1:7),]
VaR_FZG = read.csv("VaR_FZG.csv",sep = " ")[-c(1:7),]

ES_AL = read.csv("ES_AL.csv",sep = " ")[-c(1:7),]
ES_NZ = read.csv("ES_NZ.csv",sep = " ")[-c(1:7),]
ES_FZG = read.csv("ES_FZG.csv",sep = " ")[-c(1:7),]

#### VaR and ES 1%
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], FZG = VaR_FZG[,1], NZ = VaR_NZ[,1], AL = VaR_AL[,1])
ES1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], FZG = ES_FZG[,1],  NZ = ES_NZ[,1], AL = ES_AL[,1])
#### VaR and ES 2.5%
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], FZG = VaR_FZG[,2],  NZ = VaR_NZ[,2], AL = VaR_AL[,2])
ES2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], FZG = ES_FZG[,2],  NZ = ES_NZ[,2], AL = ES_AL[,2])
#### VaR and ES 1%
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], FZG = VaR_FZG[,3],  NZ = VaR_NZ[,3], AL = VaR_AL[,3])
ES5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], FZG = ES_FZG[,3],  NZ = ES_NZ[,3], AL = ES_AL[,3])
colnames(ES1) = colnames(VaR1) = colnames(ES2) = colnames(VaR2) = colnames(ES5) = colnames(VaR5) = c("GAS", "MSGARCH", "RGARCHBoot", "FGZ", "NZ", "AL")


###########################################
#########   VaR Backtesting      ##########
###########################################
library(GAS)
library(esback)
library(rugarch)
library(xtable)

K = dim(VaR1)[2]

a1 = 0.010
a2 = 0.025
a5 = 0.050

BackVaRES1 = BackVaRES2 = BackVaRES5 = matrix(0,ncol = 14,nrow = K) 
colnames(BackVaRES1) = colnames(BackVaRES2) = colnames(BackVaRES5) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESRV1", "ESRV2" ,"ESRV5", "AQL", "AAL", "ANF", "AFZG")

for (i in 1:K){
  BackT1 = BacktestVaR(ret, VaR1[,i], alpha = a1, Lags = 4)
  BackT2 = BacktestVaR(ret, VaR2[,i], alpha = a2, Lags = 4)
  BackT5 = BacktestVaR(ret, VaR5[,i], alpha = a5, Lags = 4)
  
  EBackT1 = ESTest(alpha = a1, ret, ES1[,i], VaR1[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackT2 = ESTest(alpha = a2, ret, ES2[,i], VaR2[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackT5 = ESTest(alpha = a5, ret, ES5[,i], VaR5[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  
  
  BackVaRES1[i,] = c(mean(ret < VaR1[,i])*100, 
                     BackT1$LRuc[2], BackT1$LRcc[2],BackT1$DQ$pvalue, VaR_VQR(ret, VaR1[,i], a1),
                     EBackT1$boot.p.value,
                     cc_backtest(ret, VaR1[,i], ES1[,i],  alpha  = a1)$pvalue_twosided_simple, 
                     esr_backtest2(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 1)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 2)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 3)$pvalue_twosided_asymptotic,
                     mean(QL(VaR1[,i],ret, alpha = a1)),
                     mean(AL(VaR1[,i], ES1[,i], ret, alpha = a1)),
                     mean(NZ(VaR1[,i], ES1[,i], ret, alpha = a1)),
                     mean(FZG(VaR1[,i], ES1[,i], ret, alpha = a1)))
  
  BackVaRES2[i,] = c(mean(ret < VaR2[,i])*100,
                     BackT2$LRuc[2], BackT2$LRcc[2],BackT2$DQ$pvalue, VaR_VQR(ret, VaR2[,i], a2),
                     EBackT2$boot.p.value,
                     cc_backtest(ret, VaR2[,i], ES2[,i],  alpha  = a2)$pvalue_twosided_simple, 
                     esr_backtest2(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 3)$pvalue_twosided_asymptotic,
                     mean(QL(VaR2[,i],ret, alpha = a2)),
                     mean(AL(VaR2[,i], ES2[,i], ret, alpha = a2)),
                     mean(NZ(VaR2[,i], ES2[,i], ret, alpha = a2)),
                     mean(FZG(VaR2[,i], ES2[,i], ret, alpha = a2)))
  
  BackVaRES5[i,] = c(mean(ret < VaR5[,i])*100, 
                     BackT5$LRuc[2], BackT5$LRcc[2],BackT5$DQ$pvalue, VaR_VQR(ret, VaR5[,i], a5),
                     EBackT5$boot.p.value,
                     cc_backtest(ret, VaR5[,i], ES5[,i],  alpha  = a5)$pvalue_twosided_simple, 
                     esr_backtest2(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic,
                     esr_backtest2(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 3)$pvalue_twosided_asymptotic,
                     mean(QL(VaR5[,i],ret, alpha = a5)),
                     mean(AL(VaR5[,i], ES5[,i], ret, alpha = a5)),
                     mean(NZ(VaR5[,i], ES5[,i], ret, alpha = a5)),
                     mean(FZG(VaR5[,i], ES5[,i], ret, alpha = a5)))
}


VaRES = rbind(BackVaRES1,BackVaRES2,BackVaRES5)
row.names(VaRES) = rep(c("Parametric", "Bayesian", "Bootstrap", "AL", "NZ", "FZG"),3)



print(xtable(VaRES, digits = 2), file = "VaRESBTC.tex", compress = FALSE)




