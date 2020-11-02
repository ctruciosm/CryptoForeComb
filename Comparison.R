###########################################
### Forecasting Combination VaR and ES  ###
### Comparisons                         ###
###########################################

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/")


library(stringr)
library(modelconf)
library(dplyr)
library(modelconf)
library(GAS)
library(esback)
library(rugarch)
library(xtable)
library(esreg)
library(optimx)
source("Function_VaR_VQR.R")
source("esr_backtest_modified.R")
source("Optimizations.R")


p = 0.05
pMCS = 0.05

# Table 2: rl = 1 and a = 0.010 and risklevel = c("GAS1","MSGARCH1","Boot1") 
# Table 3: rl = 2 and a = 0.025 and risklevel = c("GAS2","MSGARCH2","Boot2") 
# Table 4: rl = 3 and a = 0.050 and risklevel = c("GAS5","MSGARCH5","Boot5") 
rl = 3;  a = 0.050
risklevel = c("GAS5","MSGARCH5","Boot5")  

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/BTC/")
if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
  
  OoS = 365
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  
  CovidDay = which(crypto$date == "2020-03-12")
  print(CovidDay)
  
  # Setting 
  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
  retBTC = read.csv("VaR.csv")[,"OoS"]
  
  VaR_RSC_FZG = read.csv("VaR_RSC_FZG.csv",sep = " ")
  VaR_RSC_NZ = read.csv("VaR_RSC_NZ.csv",sep = " ")
  VaR_RSC_AL = read.csv("VaR_RSC_AL.csv",sep = " ")
  
  
  ES_RSC_FZG = read.csv("ES_RSC_FZG.csv",sep = " ")
  ES_RSC_NZ = read.csv("ES_RSC_NZ.csv",sep = " ")
  ES_RSC_AL = read.csv("ES_RSC_AL.csv",sep = " ")
  
  
  VaR_MSC_FZG = read.csv("VaR_MSC_FZG.csv",sep = " ")
  VaR_MSC_NZ = read.csv("VaR_MSC_NZ.csv",sep = " ")
  VaR_MSC_AL = read.csv("VaR_MSC_AL.csv",sep = " ")
  
  
  ES_MSC_FZG = read.csv("ES_MSC_FZG.csv",sep = " ")
  ES_MSC_NZ = read.csv("ES_MSC_NZ.csv",sep = " ")
  ES_MSC_AL = read.csv("ES_MSC_AL.csv",sep = " ")
  
  VaR_AVG_AL = read.csv("VaR_AVG_AL.csv",sep = " ")
  
  ES_AVG_AL = read.csv("ES_AVG_AL.csv",sep = " ")
  
  
  names1 = c("Parametric1", "Bayesian1", "Bootstrap1", 
             "AVG1", "FGZ_RSC1", "NZ_RSC1", "AL_RSC1", "FGZ_MSC1", "NZ_MSC1", "AL_MSC1")
  
  #### VaR and ES 1%
  VaRBTC = cbind(VaR[,risklevel], 
                 AL_AVG = VaR_AVG_AL[,rl],
                 FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
                 FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ESBTC = cbind(ES[,risklevel], 
                AL_AVG = ES_AVG_AL[,rl],
                FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
                FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
}
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/ETH/")
if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
  
  OoS = 365
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  
  CovidDay = which(crypto$date == "2020-03-12")
  print(CovidDay)
  
  # Setting 
  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
  retETH = read.csv("VaR.csv")[,"OoS"]
  
  VaR_RSC_FZG = read.csv("VaR_RSC_FZG.csv",sep = " ")
  VaR_RSC_NZ = read.csv("VaR_RSC_NZ.csv",sep = " ")
  VaR_RSC_AL = read.csv("VaR_RSC_AL.csv",sep = " ")
  
  
  ES_RSC_FZG = read.csv("ES_RSC_FZG.csv",sep = " ")
  ES_RSC_NZ = read.csv("ES_RSC_NZ.csv",sep = " ")
  ES_RSC_AL = read.csv("ES_RSC_AL.csv",sep = " ")
  
  
  VaR_MSC_FZG = read.csv("VaR_MSC_FZG.csv",sep = " ")
  VaR_MSC_NZ = read.csv("VaR_MSC_NZ.csv",sep = " ")
  VaR_MSC_AL = read.csv("VaR_MSC_AL.csv",sep = " ")
  
  
  ES_MSC_FZG = read.csv("ES_MSC_FZG.csv",sep = " ")
  ES_MSC_NZ = read.csv("ES_MSC_NZ.csv",sep = " ")
  ES_MSC_AL = read.csv("ES_MSC_AL.csv",sep = " ")
  
  VaR_AVG_AL = read.csv("VaR_AVG_AL.csv",sep = " ")
  
  ES_AVG_AL = read.csv("ES_AVG_AL.csv",sep = " ")
  
  
  names1 = c("Parametric1", "Bayesian1", "Bootstrap1", 
             "AVG1", "FGZ_RSC1", "NZ_RSC1", "AL_RSC1", "FGZ_MSC1", "NZ_MSC1", "AL_MSC1")
  
  #### VaR and ES 1%
  VaRETH = cbind(VaR[,risklevel], 
                 AL_AVG = VaR_AVG_AL[,rl],
                 FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
                 FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ESETH = cbind(ES[,risklevel], 
                AL_AVG = ES_AVG_AL[,rl],
                FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
                FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
}
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/LTC/")
if(str_sub(getwd(), - 3, - 1)   == "LTC"){
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2020-09-18")
  
  OoS = 365
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  
  CovidDay = which(crypto$date == "2020-03-12")
  print(CovidDay)
  
  # Setting 
  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
  retLTC = read.csv("VaR.csv")[,"OoS"]
  
  VaR_RSC_FZG = read.csv("VaR_RSC_FZG.csv",sep = " ")
  VaR_RSC_NZ = read.csv("VaR_RSC_NZ.csv",sep = " ")
  VaR_RSC_AL = read.csv("VaR_RSC_AL.csv",sep = " ")
  
  
  ES_RSC_FZG = read.csv("ES_RSC_FZG.csv",sep = " ")
  ES_RSC_NZ = read.csv("ES_RSC_NZ.csv",sep = " ")
  ES_RSC_AL = read.csv("ES_RSC_AL.csv",sep = " ")
  
  
  VaR_MSC_FZG = read.csv("VaR_MSC_FZG.csv",sep = " ")
  VaR_MSC_NZ = read.csv("VaR_MSC_NZ.csv",sep = " ")
  VaR_MSC_AL = read.csv("VaR_MSC_AL.csv",sep = " ")
  
  
  ES_MSC_FZG = read.csv("ES_MSC_FZG.csv",sep = " ")
  ES_MSC_NZ = read.csv("ES_MSC_NZ.csv",sep = " ")
  ES_MSC_AL = read.csv("ES_MSC_AL.csv",sep = " ")
  
  VaR_AVG_AL = read.csv("VaR_AVG_AL.csv",sep = " ")
  
  ES_AVG_AL = read.csv("ES_AVG_AL.csv",sep = " ")
  
  
  names1 = c("Parametric1", "Bayesian1", "Bootstrap1", 
             "AVG1", "FGZ_RSC1", "NZ_RSC1", "AL_RSC1", "FGZ_MSC1", "NZ_MSC1", "AL_MSC1")
  
  #### VaR and ES 1%
  VaRLTC = cbind(VaR[,risklevel], 
                 AL_AVG = VaR_AVG_AL[,rl],
                 FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
                 FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ESLTC = cbind(ES[,risklevel], 
                AL_AVG = ES_AVG_AL[,rl],
                FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
                FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
}
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/XRP/")
if(str_sub(getwd(), - 3, - 1)   == "XRP"){
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2020-09-18") 
  
  OoS = 365
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  
  CovidDay = which(crypto$date == "2020-03-12")
  print(CovidDay)
  
  # Setting 
  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
  retXRP = read.csv("VaR.csv")[,"OoS"]
  
  VaR_RSC_FZG = read.csv("VaR_RSC_FZG.csv",sep = " ")
  VaR_RSC_NZ = read.csv("VaR_RSC_NZ.csv",sep = " ")
  VaR_RSC_AL = read.csv("VaR_RSC_AL.csv",sep = " ")
  
  
  ES_RSC_FZG = read.csv("ES_RSC_FZG.csv",sep = " ")
  ES_RSC_NZ = read.csv("ES_RSC_NZ.csv",sep = " ")
  ES_RSC_AL = read.csv("ES_RSC_AL.csv",sep = " ")
  
  
  VaR_MSC_FZG = read.csv("VaR_MSC_FZG.csv",sep = " ")
  VaR_MSC_NZ = read.csv("VaR_MSC_NZ.csv",sep = " ")
  VaR_MSC_AL = read.csv("VaR_MSC_AL.csv",sep = " ")
  
  
  ES_MSC_FZG = read.csv("ES_MSC_FZG.csv",sep = " ")
  ES_MSC_NZ = read.csv("ES_MSC_NZ.csv",sep = " ")
  ES_MSC_AL = read.csv("ES_MSC_AL.csv",sep = " ")
  
  VaR_AVG_AL = read.csv("VaR_AVG_AL.csv",sep = " ")
  
  ES_AVG_AL = read.csv("ES_AVG_AL.csv",sep = " ")
  
  
  names1 = c("Parametric1", "Bayesian1", "Bootstrap1", 
             "AVG1", "FGZ_RSC1", "NZ_RSC1", "AL_RSC1", "FGZ_MSC1", "NZ_MSC1", "AL_MSC1")
  
  #### VaR and ES 1%
  VaRXRP = cbind(VaR[,risklevel], 
                 AL_AVG = VaR_AVG_AL[,rl],
                 FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
                 FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ESXRP = cbind(ES[,risklevel], 
                AL_AVG = ES_AVG_AL[,rl],
                FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
                FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
}







###########################################
#########   VaR Backtesting      ##########
###########################################
K = dim(VaRBTC)[2]

BackVaRESBTC = BackVaRESETH = BackVaRESLTC = BackVaRESXRP= matrix(0,ncol = 18,nrow = K) 
colnames(BackVaRESBTC) = colnames(BackVaRESETH) = colnames(BackVaRESLTC) = colnames(BackVaRESXRP) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESR_1", "ESR_2" ,"ESR_3", "AQL", "AFZG", "ANZ", "AAL","AQL2", "AFZG2", "ANZ2", "AAL2")

for (i in 1:K){
  BackTBTC = BacktestVaR(retBTC, VaRBTC[,i], alpha = a, Lags = 4)
  BackTETH = BacktestVaR(retETH, VaRETH[,i], alpha = a, Lags = 4)
  BackTLTC = BacktestVaR(retLTC, VaRLTC[,i], alpha = a, Lags = 4)
  BackTXRP = BacktestVaR(retXRP, VaRXRP[,i], alpha = a, Lags = 4)
  
  EBackTBTC = ESTest(alpha = a, retBTC, ESBTC[,i], VaRBTC[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH = ESTest(alpha = a, retETH, ESETH[,i], VaRETH[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTLTC = ESTest(alpha = a, retLTC, ESLTC[,i], VaRLTC[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTXRP = ESTest(alpha = a, retXRP, ESXRP[,i], VaRXRP[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  
  
  BackVaRESBTC[i,] = c(mean(retBTC < VaRBTC[,i])*100, 
                       BackTBTC$LRuc[2], BackTBTC$LRcc[2],BackTBTC$DQ$pvalue, VaR_VQR(retBTC, VaRBTC[,i], a),
                       EBackTBTC$boot.p.value,
                       cc_backtest(retBTC, VaRBTC[,i], ESBTC[,i],  alpha  = a)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC[,i], ESBTC[,i],alpha  = a, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC[,i], ESBTC[,i],alpha  = a, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC[,i], ESBTC[,i],alpha  = a, B = 0, version = 3)$pvalue_twosided_asymptotic),
                       mean(QL(VaRBTC[,i],retBTC, alpha = a)),
                       mean(FZG(VaRBTC[,i], ESBTC[,i], retBTC, alpha = a)),
                       mean(NZ(VaRBTC[,i], ESBTC[,i], retBTC, alpha = a)),
                       mean(AL(VaRBTC[,i], ESBTC[,i], retBTC, alpha = a)),
                       mean(QL(VaRBTC[-CovidDay,i],retBTC[-CovidDay], alpha = a)),
                       mean(FZG(VaRBTC[-CovidDay,i], ESBTC[-CovidDay,i], retBTC[-CovidDay], alpha = a)),
                       mean(NZ(VaRBTC[-CovidDay,i], ESBTC[-CovidDay,i], retBTC[-CovidDay], alpha = a)),
                       mean(AL(VaRBTC[-CovidDay,i], ESBTC[-CovidDay,i], retBTC[-CovidDay], alpha = a)))
  
  BackVaRESETH[i,] = c(mean(retETH < VaRETH[,i])*100, 
                       BackTETH$LRuc[2], BackTETH$LRcc[2],BackTETH$DQ$pvalue, VaR_VQR(retETH, VaRETH[,i], a),
                       EBackTETH$boot.p.value,
                       cc_backtest(retETH, VaRETH[,i], ESETH[,i],  alpha  = a)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH[,i], ESETH[,i],alpha  = a, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH[,i], ESETH[,i],alpha  = a, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH[,i], ESETH[,i],alpha  = a, B = 0, version = 3)$pvalue_twosided_asymptotic),
                       mean(QL(VaRETH[,i],retETH, alpha = a)),
                       mean(FZG(VaRETH[,i], ESETH[,i], retETH, alpha = a)),
                       mean(NZ(VaRETH[,i], ESETH[,i], retETH, alpha = a)),
                       mean(AL(VaRETH[,i], ESETH[,i], retETH, alpha = a)),
                       mean(QL(VaRETH[-CovidDay,i],retETH[-CovidDay], alpha = a)),
                       mean(FZG(VaRETH[-CovidDay,i], ESETH[-CovidDay,i], retETH[-CovidDay], alpha = a)),
                       mean(NZ(VaRETH[-CovidDay,i], ESETH[-CovidDay,i], retETH[-CovidDay], alpha = a)),
                       mean(AL(VaRETH[-CovidDay,i], ESETH[-CovidDay,i], retETH[-CovidDay], alpha = a)))
  
  BackVaRESLTC[i,] = c(mean(retLTC < VaRLTC[,i])*100, 
                       BackTLTC$LRuc[2], BackTLTC$LRcc[2],BackTLTC$DQ$pvalue, VaR_VQR(retLTC, VaRLTC[,i], a),
                       EBackTLTC$boot.p.value,
                       cc_backtest(retLTC, VaRLTC[,i], ESLTC[,i],  alpha  = a)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retLTC, VaRLTC[,i], ESLTC[,i],alpha  = a, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retLTC, VaRLTC[,i], ESLTC[,i],alpha  = a, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retLTC, VaRLTC[,i], ESLTC[,i],alpha  = a, B = 0, version = 3)$pvalue_twosided_asymptotic),
                       mean(QL(VaRLTC[,i],retLTC, alpha = a)),
                       mean(FZG(VaRLTC[,i], ESLTC[,i], retLTC, alpha = a)),
                       mean(NZ(VaRLTC[,i], ESLTC[,i], retLTC, alpha = a)),
                       mean(AL(VaRLTC[,i], ESLTC[,i], retLTC, alpha = a)),
                       mean(QL(VaRLTC[-CovidDay,i],retLTC[-CovidDay], alpha = a)),
                       mean(FZG(VaRLTC[-CovidDay,i], ESLTC[-CovidDay,i], retLTC[-CovidDay], alpha = a)),
                       mean(NZ(VaRLTC[-CovidDay,i], ESLTC[-CovidDay,i], retLTC[-CovidDay], alpha = a)),
                       mean(AL(VaRLTC[-CovidDay,i], ESLTC[-CovidDay,i], retLTC[-CovidDay], alpha = a)))
  
  
  BackVaRESXRP[i,] = c(mean(retXRP < VaRXRP[,i])*100, 
                       BackTXRP$LRuc[2], BackTXRP$LRcc[2],BackTXRP$DQ$pvalue, VaR_VQR(retXRP, VaRXRP[,i], a),
                       EBackTXRP$boot.p.value,
                       cc_backtest(retXRP, VaRXRP[,i], ESXRP[,i],  alpha  = a)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retXRP, VaRXRP[,i], ESXRP[,i],alpha  = a, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retXRP, VaRXRP[,i], ESXRP[,i],alpha  = a, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retXRP, VaRXRP[,i], ESXRP[,i],alpha  = a, B = 0, version = 3)$pvalue_twosided_asymptotic),
                       mean(QL(VaRXRP[,i],retXRP, alpha = a)),
                       mean(FZG(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(NZ(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(AL(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(QL(VaRXRP[-CovidDay,i],retXRP[-CovidDay], alpha = a)),
                       mean(FZG(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)),
                       mean(NZ(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)),
                       mean(AL(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)))
                     
                     
  
}


VaRES = rbind(BackVaRESBTC,BackVaRESETH,BackVaRESLTC,BackVaRESXRP)
names = c("Param.", "Bayes.", "Boots.", "AVG", "RSC_FGZ", "RSC_NZ", "RSC_AL", "MSC_FGZ", "MSC_NZ", "MSC_AL")
row.names(VaRES) = c(names,names,names,names)
VaRES_AUX = VaRES
VaRES = VaRES %>% data.frame()


VaRES$UC = ifelse(VaRES$UC>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$UC,2),nsmall = 2)), format(round(VaRES$UC,2),nsmall = 2))
VaRES$CC = ifelse(VaRES$CC>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$CC,2),nsmall = 2)), format(round(VaRES$CC,2),nsmall = 2))
VaRES$DQ = ifelse(VaRES$DQ>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$DQ,2),nsmall = 2)), format(round(VaRES$DQ,2),nsmall = 2))
VaRES$VQ = ifelse(VaRES$VQ>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$VQ,2),nsmall = 2)), format(round(VaRES$VQ,2),nsmall = 2))
VaRES$MFE = ifelse(VaRES$MFE>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$MFE,2),nsmall = 2)), format(round(VaRES$MFE,2),nsmall = 2))
VaRES$NZ = ifelse(VaRES$NZ>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$NZ,2),nsmall = 2)), format(round(VaRES$NZ,2),nsmall = 2))
VaRES$ESR_1 = ifelse(VaRES$ESR_1>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$ESR_1,2),nsmall = 2)), format(round(VaRES$ESR_1,2),nsmall = 2))
VaRES$ESR_2 = ifelse(VaRES$ESR_2>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$ESR_2,2),nsmall = 2)), format(round(VaRES$ESR_2,2),nsmall = 2))
VaRES$ESR_3 = ifelse(VaRES$ESR_3>p,paste0('\\cellcolor{gray!25}',format(round(VaRES$ESR_3,2),nsmall = 2)), format(round(VaRES$ESR_3,2),nsmall = 2))
VaRES$Hits = format(round(VaRES$Hits,1),nsmall = 1)


namesBTC = c("Param.BTC", "Bayes.BTC", "Boots.BTC", "AVGBTC", "RSC_FGZBTC", "RSC_NZBTC", "RSC_ALBTC", "MSC_FGZBTC", "MSC_NZBTC", "MSC_ALBTC")
namesETH = c("Param.ETH", "Bayes.ETH", "Boots.ETH", "AVGETH", "RSC_FGZETH", "RSC_NZETH", "RSC_ALETH", "MSC_FGZETH", "MSC_NZETH", "MSC_ALETH")
namesLTC = c("Param.LTC", "Bayes.LTC", "Boots.LTC", "AVGLTC", "RSC_FGZLTC", "RSC_NZLTC", "RSC_ALLTC", "MSC_FGZLTC", "MSC_NZLTC", "MSC_ALLTC")
namesXRP = c("Param.XRP", "Bayes.XRP", "Boots.XRP", "AVGXRP", "RSC_FGZXRP", "RSC_NZXRP", "RSC_ALXRP", "MSC_FGZXRP", "MSC_NZXRP", "MSC_ALXRP")

Auxmatrix = matrix(0,ncol=length(row.names(VaRES_AUX)), nrow = 1)
colnames(Auxmatrix) = c(namesBTC,namesETH,namesLTC,namesXRP)
  
AuxBTC = Auxmatrix %>% data.frame() %>% select(ends_with("BTC"))
AuxETH = Auxmatrix %>% data.frame() %>% select(ends_with("ETH"))
AuxLTC = Auxmatrix %>% data.frame() %>% select(ends_with("LTC"))
AuxXRP = Auxmatrix %>% data.frame() %>% select(ends_with("XRP"))
  
MCSBTC_MQL = rep(0,length(AuxBTC))
MCSETH_MQL = rep(0,length(AuxETH))
MCSLTC_MQL = rep(0,length(AuxLTC))
MCSXRP_MQL = rep(0,length(AuxXRP))
  
MCSBTC_MFZG = rep(0,length(AuxBTC))
MCSETH_MFZG = rep(0,length(AuxETH))
MCSLTC_MFZG = rep(0,length(AuxLTC))
MCSXRP_MFZG = rep(0,length(AuxXRP))
  
MCSBTC_MNZ = rep(0,length(AuxBTC))
MCSETH_MNZ = rep(0,length(AuxETH))
MCSLTC_MNZ = rep(0,length(AuxLTC))
MCSXRP_MNZ = rep(0,length(AuxXRP))
  
MCSBTC_MAL = rep(0,length(AuxBTC))
MCSETH_MAL = rep(0,length(AuxETH))
MCSLTC_MAL = rep(0,length(AuxLTC))
MCSXRP_MAL = rep(0,length(AuxXRP))
  

  
  
if (ncol(AuxBTC)>1){
  MQL = QL(VaRBTC,retBTC, alpha = a)
  colnames(MQL) = colnames(AuxBTC)
  auxBTC_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
  MCSBTC_MQL[auxBTC_MQL] = 1

  MFZG = FZG(VaRBTC,ESBTC, retBTC, alpha = a)
  colnames(MFZG) = colnames(AuxBTC)
  auxBTC_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
  MCSBTC_MFZG[auxBTC_MFZG] = 1 
  
  MNZ = NZ(VaRBTC,ESBTC, retBTC, alpha = a)
  colnames(MNZ) = colnames(AuxBTC)
  auxBTC_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
  MCSBTC_MNZ[auxBTC_MNZ] = 1 
  
  
  MAL = AL(VaRBTC,ESBTC, retBTC, alpha = a)
  colnames(MAL) = colnames(AuxBTC)
  auxBTC_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
  MCSBTC_MAL[auxBTC_MAL] = 1
} 
  
if (ncol(AuxETH)>1){
    MQL = QL(VaRETH,retETH, alpha = a)
    colnames(MQL) = colnames(AuxETH)
    auxETH_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSETH_MQL[auxETH_MQL] = 1
    
    MFZG = FZG(VaRETH,ESETH, retETH, alpha = a)
    colnames(MFZG) = colnames(AuxETH)
    auxETH_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSETH_MFZG[auxETH_MFZG] = 1 
    
    MNZ = NZ(VaRETH,ESETH, retETH, alpha = a)
    colnames(MNZ) = colnames(AuxETH)
    auxETH_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSETH_MNZ[auxETH_MNZ] = 1 
    
    MAL = AL(VaRETH,ESETH, retETH, alpha = a)
    colnames(MAL) = colnames(AuxETH)
    auxETH_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSETH_MAL[auxETH_MAL] = 1
} 
  
if (ncol(AuxLTC)>1){
    MQL = QL(VaRLTC,retLTC, alpha = a)
    colnames(MQL) = colnames(AuxLTC)
    auxLTC_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSLTC_MQL[auxLTC_MQL] = 1
    
    MFZG = FZG(VaRLTC,ESLTC, retLTC, alpha = a)
    colnames(MFZG) = colnames(AuxLTC)
    auxLTC_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSLTC_MFZG[auxLTC_MFZG] = 1 
    
    MNZ = NZ(VaRLTC,ESLTC, retLTC, alpha = a)
    colnames(MNZ) = colnames(AuxLTC)
    auxLTC_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSLTC_MNZ[auxLTC_MNZ] = 1 
    
    MAL = AL(VaRLTC,ESLTC, retLTC, alpha = a)
    colnames(MAL) = colnames(AuxLTC)
    auxLTC_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSLTC_MAL[auxLTC_MAL] = 1
  } 
  
if (ncol(AuxXRP)>1){
    MQL = QL(VaRXRP,retXRP, alpha = a)
    colnames(MQL) = colnames(AuxXRP)
    auxXRP_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSXRP_MQL[auxXRP_MQL] = 1
    
    MFZG = FZG(VaRXRP,ESXRP, retXRP, alpha = a)
    colnames(MFZG) = colnames(AuxXRP)
    auxXRP_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSXRP_MFZG[auxXRP_MFZG] = 1 
    
    MNZ = NZ(VaRXRP,ESXRP, retXRP, alpha = a)
    colnames(MNZ) = colnames(AuxXRP)
    auxXRP_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSXRP_MNZ[auxXRP_MNZ] = 1 
    
    MAL = AL(VaRXRP,ESXRP, retXRP, alpha = a)
    colnames(MAL) = colnames(AuxXRP)
    auxXRP_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
    MCSXRP_MAL[auxXRP_MAL] = 1
  } 



  
  
  
  MCCQL = c(MCSBTC_MQL,MCSETH_MQL,MCSLTC_MQL,MCSXRP_MQL)
  MCCFZG = c(MCSBTC_MFZG,MCSETH_MFZG,MCSLTC_MFZG,MCSXRP_MFZG)
  MCCNZ = c(MCSBTC_MNZ,MCSETH_MNZ,MCSLTC_MNZ,MCSXRP_MNZ)
  MCCAL = c(MCSBTC_MAL,MCSETH_MAL,MCSLTC_MAL,MCSXRP_MAL) 
  
  VaRES_AUX = VaRES_AUX %>% data.frame() %>% mutate(MCCQL = MCCQL, MCCFZG = MCCFZG, MCCNZ = MCCNZ, MCCAL = MCCAL)
  row.names(VaRES_AUX) = row.names(VaRES_AUX)
  
  VaRES$AQL = ifelse(VaRES_AUX$MCCQL>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AQL,2),nsmall = 2)), format(round(VaRES$AQL,2),nsmall = 2))
  VaRES$AFZG  = ifelse(VaRES_AUX$MCCFZG>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AFZG,2),nsmall = 2)), format(round(VaRES$AFZG,2),nsmall = 2))
  VaRES$ANZ = ifelse(VaRES_AUX$MCCNZ>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$ANZ,2),nsmall = 2)), format(round(VaRES$ANZ,2),nsmall = 2))
  VaRES$AAL = ifelse(VaRES_AUX$MCCAL>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AAL,2),nsmall = 2)), format(round(VaRES$AAL,2),nsmall = 2))
  
  VaRES$AQL2 = format(round(VaRES$AQL2,2),nsmall = 2)
  VaRES$AFZG2 = format(round(VaRES$AFZG2,2),nsmall = 2)
  VaRES$ANZ2 = format(round(VaRES$ANZ2,2),nsmall = 2)
  VaRES$AAL2 = format(round(VaRES$AAL2,2),nsmall = 2)
  
  
  VaRES = VaRES %>% select(-UC,-DQ, -ESR_1,-ESR_2,-AQL2,-AFZG2,-ANZ2,-AAL2)
  

  
  VaRES_AUX$label =rep(1:10,4)
  VaRES_AUX$crypto = c(rep("BTC",10),rep("ETH",10),rep("LTC",10), rep("XRP",10))
  
  
  
  summarised = VaRES_AUX %>% mutate(UC = ifelse(UC>p,1,0),
                   CC = ifelse(CC>p,1,0),
                   DQ = ifelse(DQ>p,1,0),
                   VQ = ifelse(VQ>p,1,0),
                   MFE = ifelse(MFE>p,1,0),
                   NZ = ifelse(NZ>p,1,0),
                   ESR_1 = ifelse(ESR_1>p,1,0),
                   ESR_2 = ifelse(ESR_2>p,1,0),
                   ESR_3 = ifelse(ESR_3>p,1,0)) %>% 
    group_by(label) %>% 
    summarize(Hits = sum(Hits),
              UC = sum(UC),
              CC = sum(CC),
              DQ = sum(DQ),
              VQ = sum(VQ),
              MFE = sum(MFE),
              NZ = sum(NZ),
              ESR_1 = sum(ESR_1),
              ESR_2 = sum(ESR_2),
              ESR_3 = sum(ESR_3),
              AQL = sum(MCCQL),
              AFZG = sum(MCCFZG),
              ANZ = sum(MCCNZ),
              AAL = sum(MCCAL)) %>% select(-label, -UC, -DQ, -ESR_1, -ESR_2) %>% data.frame()
  
  
VaRES = rbind(VaRES,summarised)  
row.names(VaRES) = c(namesBTC, namesETH, namesLTC, namesXRP,names)

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/")

Caption = "One-step-ahead VaR and ES backtesting for BTC at 1\\% (top panel), 2.5\\% (middle panel) and 5\\% (bottom panel) risk levels. Shaded cells in the calibration test indicate $p$-values larger than 0.01 while shaded cells in the scoring function stand for models in the MCS."
print(xtable(VaRES, caption = Caption,  align = "l|ccc|ccc|cccc"), file = "VaRES5.tex", compress = FALSE)


