################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### Assessment procedures' performance
#### This code produces the comparison Tables in the paper
################################################################################## 

VaR_ES_full = function(risklevel, rl){
  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
  ret_OoS = read.csv("VaR.csv")[,"OoS"]
  
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
  
  VaR_full = VaR %>% data.frame() %>% 
    select(ends_with(risklevel), -contains("MIXTURE")) %>%
    mutate(FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
           FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ES_full = ES %>% data.frame() %>% 
    select(ends_with(risklevel), -contains("MIXTURE")) %>%
    mutate(FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
           FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
  return(list(ret_OoS,VaR_full, ES_full))
}

n_ind = 5 # number os individual models
setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/CryptoForeComb/")

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
library(kableExtra)
source("Function_VaR_VQR.R")
source("esr_backtest_modified.R")
source("Optimizations.R")
source("00_GiacominiRossi.R")


p = 0.05
pMCS = 0.25
mu_ = 0.1
# Table 2: rl = 1 and a = 0.010 and risklevel = as.character(1)
# Table 3: rl = 2 and a = 0.025 and risklevel = as.character(2)
# Table 4: rl = 3 and a = 0.050 and risklevel = as.character(5)
# Table 5: rl = 4 and a = 0.100 and risklevel = as.character(10)
rl = 4;  a = 0.100; risklevel = as.character(10)
Caption = "One-step-ahead VaR and ES backtesting for BTC, ETH, LTC and XRP for the 10\\% risk level. Shaded rows indicate procedures with p-values larger than 0.05 in all calibration tests. $^{G,M,B,F,A}$ superscripts in the combining methods stand for procedures outperforming the GAS(G), MSGARCH(M), Bootstrap(B), FIGARCH(F) and/or AVGARCH(A) methods, respectively (according to the Giacomini and Rossi (2010) fluctuation test at 0.1 significance level)."
file_tex_name = "VaRES10.tex" 
label_name = "Table_VaRES10"



setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/BTC/")
if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(risklevel, rl)
  retBTC = AUX[[1]]
  VaRBTC = AUX[[2]]
  ESBTC  = AUX[[3]]
}
setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/ETH/")
if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(risklevel, rl)
  retETH = AUX[[1]]
  VaRETH = AUX[[2]]
  ESETH  = AUX[[3]]
}
setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/LTC/")
if(str_sub(getwd(), - 3, - 1)   == "LTC"){
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2021-06-29")
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(risklevel, rl)
  retLTC = AUX[[1]]
  VaRLTC = AUX[[2]]
  ESLTC  = AUX[[3]]
}
setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/XRP/")
if(str_sub(getwd(), - 3, - 1)   == "XRP"){
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2021-06-29") 
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(risklevel, rl)
  retXRP = AUX[[1]]
  VaRXRP = AUX[[2]]
  ESXRP  = AUX[[3]]
}

###########################################
# Including Basic Combining Strategies
### Mean
VaRBTC$AVG = apply(VaRBTC[,1:n_ind],1,mean)
VaRETH$AVG = apply(VaRETH[,1:n_ind],1,mean)
VaRLTC$AVG = apply(VaRLTC[,1:n_ind],1,mean)
VaRXRP$AVG = apply(VaRXRP[,1:n_ind],1,mean)

ESBTC$AVG = apply(ESBTC[,1:n_ind],1,mean)
ESETH$AVG = apply(ESETH[,1:n_ind],1,mean)
ESLTC$AVG = apply(ESLTC[,1:n_ind],1,mean)
ESXRP$AVG = apply(ESXRP[,1:n_ind],1,mean)


### Median
VaRBTC$MED = apply(VaRBTC[,1:n_ind],1,median)
VaRETH$MED = apply(VaRETH[,1:n_ind],1,median)
VaRLTC$MED = apply(VaRLTC[,1:n_ind],1,median)
VaRXRP$MED = apply(VaRXRP[,1:n_ind],1,median)

ESBTC$MED = apply(ESBTC[,1:n_ind],1,median)
ESETH$MED = apply(ESETH[,1:n_ind],1,median)
ESLTC$MED = apply(ESLTC[,1:n_ind],1,median)
ESXRP$MED = apply(ESXRP[,1:n_ind],1,median)


### Min
VaRBTC$MIN = apply(VaRBTC[,1:n_ind],1,min)
VaRETH$MIN = apply(VaRETH[,1:n_ind],1,min)
VaRLTC$MIN = apply(VaRLTC[,1:n_ind],1,min)
VaRXRP$MIN = apply(VaRXRP[,1:n_ind],1,min)

ESBTC$MIN = apply(ESBTC[,1:n_ind],1,min)
ESETH$MIN = apply(ESETH[,1:n_ind],1,min)
ESLTC$MIN = apply(ESLTC[,1:n_ind],1,min)
ESXRP$MIN = apply(ESXRP[,1:n_ind],1,min)


### Max
VaRBTC$MAX = apply(VaRBTC[,1:n_ind],1,max)
VaRETH$MAX = apply(VaRETH[,1:n_ind],1,max)
VaRLTC$MAX = apply(VaRLTC[,1:n_ind],1,max)
VaRXRP$MAX = apply(VaRXRP[,1:n_ind],1,max)

ESBTC$MAX = apply(ESBTC[,1:n_ind],1,max)
ESETH$MAX = apply(ESETH[,1:n_ind],1,max)
ESLTC$MAX = apply(ESLTC[,1:n_ind],1,max)
ESXRP$MAX = apply(ESXRP[,1:n_ind],1,max)

# Including joint VaR and ES regression combining framework





###########################################
# Reordering

ordering <- c(paste0("GAS",risklevel),paste0("MSGARCH",risklevel),paste0("Boot",risklevel),
              paste0("FIGARCH",risklevel),paste0("AVGARCH",risklevel),
              "AVG","MED","MIN","MAX",
              "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

names = c("GAS", "MSGARCH", "Boot.", "FIGARCH", "AVGARCH",
          "AVG", "MED", "MIN", "MAX",
          "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

VaRBTC <- VaRBTC %>% select(ordering)
colnames(VaRBTC) = names
VaRETH <- VaRETH %>% select(ordering)
colnames(VaRETH) = names
VaRLTC <- VaRLTC %>% select(ordering)
colnames(VaRLTC) = names
VaRXRP <- VaRXRP %>% select(ordering)
colnames(VaRXRP) = names

ESBTC <- ESBTC %>% select(ordering)
colnames(ESBTC) = names
ESETH <- ESETH %>% select(ordering)
colnames(ESETH) = names
ESLTC <- ESLTC %>% select(ordering)
colnames(ESLTC) = names
ESXRP <- ESXRP %>% select(ordering)
colnames(ESXRP) = names


###########################################
#########   VaR Backtesting      ##########
###########################################
K = dim(VaRETH)[2]  

BackVaRESBTC = BackVaRESETH = BackVaRESLTC = BackVaRESXRP= matrix(0,ncol = 18,nrow = K) 
colnames(BackVaRESBTC) = colnames(BackVaRESETH) = colnames(BackVaRESLTC) = colnames(BackVaRESXRP) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESR_1", "ESR_2" ,"ESR_3", "AQL", "AFZG", "ANZ", "AAL","AQL2", "AFZG2", "ANZ2", "AAL2")

for (i in 1:K){ # each i is a method (individual or combination)
  set.seed(6532)
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
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC[,i], ESBTC[,i],alpha  = a, B = 0, version = 3)$pvalue_onesided_asymptotic),
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
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH[,i], ESETH[,i],alpha  = a, B = 0, version = 3)$pvalue_onesided_asymptotic),
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
                       suppressWarnings(esr_backtest_modified(retLTC, VaRLTC[,i], ESLTC[,i],alpha  = a, B = 0, version = 3)$pvalue_onesided_asymptotic),
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
                       suppressWarnings(esr_backtest_modified(retXRP, VaRXRP[,i], ESXRP[,i],alpha  = a, B = 0, version = 3)$pvalue_onesided_asymptotic),
                       mean(QL(VaRXRP[,i],retXRP, alpha = a)),
                       mean(FZG(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(NZ(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(AL(VaRXRP[,i], ESXRP[,i], retXRP, alpha = a)),
                       mean(QL(VaRXRP[-CovidDay,i],retXRP[-CovidDay], alpha = a)),
                       mean(FZG(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)),
                       mean(NZ(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)),
                       mean(AL(VaRXRP[-CovidDay,i], ESXRP[-CovidDay,i], retXRP[-CovidDay], alpha = a)))
}


###########################################
#######    Model Confidence Set      ######
###########################################

# MCSBTC_MQL = rep(0,ncol(VaRBTC))
# MQL = QL(VaRBTC,retBTC, alpha = a)
# colnames(MQL) = colnames(VaRBTC)
# auxBTC_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSBTC_MQL[auxBTC_MQL] = 1
# 
#   
# MCSBTC_MFZG = rep(0,ncol(VaRBTC))
# MFZG = FZG(VaRBTC,ESBTC, retBTC, alpha = a)
# colnames(MFZG) = colnames(VaRBTC)
# auxBTC_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSBTC_MFZG[auxBTC_MFZG] = 1 
#   
# MCSBTC_MNZ = rep(0,ncol(VaRBTC))
# MNZ = NZ(VaRBTC,ESBTC, retBTC, alpha = a)
# colnames(MNZ) = colnames(VaRBTC)
# auxBTC_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSBTC_MNZ[auxBTC_MNZ] = 1 
#   
# MCSBTC_MAL = rep(0,ncol(VaRBTC))
# MAL = AL(VaRBTC,ESBTC, retBTC, alpha = a)
# colnames(MAL) = colnames(VaRBTC)
# auxBTC_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSBTC_MAL[auxBTC_MAL] = 1
# 
# MCSETH_MQL = rep(0,ncol(VaRETH))
# MQL = QL(VaRETH,retETH, alpha = a)
# colnames(MQL) = colnames(VaRETH)
# auxETH_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSETH_MQL[auxETH_MQL] = 1
#  
# MCSETH_MFZG = rep(0,ncol(VaRETH))
# MFZG = FZG(VaRETH,ESETH, retETH, alpha = a)
# colnames(MFZG) = colnames(VaRETH)
# auxETH_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSETH_MFZG[auxETH_MFZG] = 1 
# 
# MCSETH_MNZ = rep(0,ncol(VaRETH))
# MNZ = NZ(VaRETH,ESETH, retETH, alpha = a)
# colnames(MNZ) = colnames(VaRETH)
# auxETH_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSETH_MNZ[auxETH_MNZ] = 1 
#   
# MCSETH_MAL = rep(0,ncol(VaRETH))
# MAL = AL(VaRETH,ESETH, retETH, alpha = a)
# colnames(MAL) = colnames(VaRETH)
# auxETH_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSETH_MAL[auxETH_MAL] = 1
#  
# 
# MCSLTC_MQL = rep(0,ncol(VaRLTC))
# MQL = QL(VaRLTC,retLTC, alpha = a)
# colnames(MQL) = colnames(VaRLTC)
# auxLTC_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSLTC_MQL[auxLTC_MQL] = 1
#   
# MCSLTC_MFZG = rep(0,ncol(VaRLTC))
# MFZG = FZG(VaRLTC,ESLTC, retLTC, alpha = a)
# colnames(MFZG) = colnames(VaRLTC)
# auxLTC_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSLTC_MFZG[auxLTC_MFZG] = 1 
#  
# MCSLTC_MNZ = rep(0,ncol(VaRLTC))
# MNZ = NZ(VaRLTC,ESLTC, retLTC, alpha = a)
# colnames(MNZ) = colnames(VaRLTC)
# auxLTC_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSLTC_MNZ[auxLTC_MNZ] = 1 
#   
# MCSLTC_MAL = rep(0,ncol(VaRLTC))
# MAL = AL(VaRLTC,ESLTC, retLTC, alpha = a)
# colnames(MAL) = colnames(VaRLTC)
# auxLTC_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSLTC_MAL[auxLTC_MAL] = 1
#  
# 
# MCSXRP_MQL = rep(0,ncol(VaRXRP))
# MQL = QL(VaRXRP,retXRP, alpha = a)
# colnames(MQL) = colnames(VaRXRP)
# auxXRP_MQL = estMCS.quick(MQL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSXRP_MQL[auxXRP_MQL] = 1
#  
# MCSXRP_MFZG = rep(0,ncol(VaRXRP))
# MFZG = FZG(VaRXRP,ESXRP, retXRP, alpha = a)
# colnames(MFZG) = colnames(VaRXRP)
# auxXRP_MFZG = estMCS.quick(MFZG, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSXRP_MFZG[auxXRP_MFZG] = 1 
#   
# MCSXRP_MNZ = rep(0,ncol(VaRXRP))
# MNZ = NZ(VaRXRP,ESXRP, retXRP, alpha = a)
# colnames(MNZ) = colnames(VaRXRP)
# auxXRP_MNZ = estMCS.quick(MNZ, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSXRP_MNZ[auxXRP_MNZ] = 1 
#  
# MCSXRP_MAL = rep(0,ncol(VaRXRP))
# MAL = AL(VaRXRP,ESXRP, retXRP, alpha = a)
# colnames(MAL) = colnames(VaRXRP)
# auxXRP_MAL = estMCS.quick(MAL, test="t.max", B=5000, l=12, alpha = pMCS)
# MCSXRP_MAL[auxXRP_MAL] = 1
#  
# 
# MCCQL = c(MCSBTC_MQL,MCSETH_MQL,MCSLTC_MQL,MCSXRP_MQL)
# MCCFZG = c(MCSBTC_MFZG,MCSETH_MFZG,MCSLTC_MFZG,MCSXRP_MFZG)
# MCCNZ = c(MCSBTC_MNZ,MCSETH_MNZ,MCSLTC_MNZ,MCSXRP_MNZ)
# MCCAL = c(MCSBTC_MAL,MCSETH_MAL,MCSLTC_MAL,MCSXRP_MAL) 
# 

#############################################
### Giacomini and Rossi Fluctuation Test  ###
#############################################

VaRES = rbind(BackVaRESBTC,BackVaRESETH,BackVaRESLTC,BackVaRESXRP) %>% 
   data.frame() %>% 
   mutate(methods = c(names, names, names,names)) %>% 
   mutate(moeda = c(rep("BTC",13), rep("ETH",13), rep("LTC",13), rep("XRP",13))) %>% 
   mutate(how_many_ct = ifelse(CC>p &  DQ > p  & MFE>p & NZ>p & ESR_2>p,1,0)) %>% 
   mutate(classe = rep(c(rep("Indiv.",5), rep("Comb.",8)),4)) %>% 
   mutate(QLGAS = 0, QLMSGARCH = 0, QLBoot. = 0, QLFIGARCH = 0, QLAVGARCH = 0,
          QLAVG = 0, QLMED = 0, QLFZG_RSC = 0, QLNZ_RSC = 0, QLAL_RSC = 0, QLFZG_MSC = 0, QLNZ_MSC = 0, QLAL_MSC = 0,
          FZGGAS = 0, FZGMSGARCH = 0, FZGBoot. = 0, FZGFIGARCH = 0, FZGAVGARCH = 0,
          FZGAVG = 0, FZGMED = 0, FZGFZG_RSC = 0, FZGNZ_RSC = 0, FZGAL_RSC = 0, FZGFZG_MSC = 0, FZGNZ_MSC = 0, FZGAL_MSC = 0,
          NZGAS = 0, NZMSGARCH = 0, NZBoot. = 0, NZFIGARCH = 0, NZAVGARCH = 0,
          NZAVG = 0, NZMMED = 0, NZFZG_RSC = 0, NZNZ_RSC = 0, NZAL_RSC = 0, NZFZG_MSC = 0, NZNZ_MSC = 0, NZAL_MSC = 0,
          ALGAS = 0, ALMSGARCH = 0, ALBoot. = 0, ALFIGARCH = 0, ALAVGARCH = 0,
          ALAVG = 0, ALMED = 0, ALFZG_RSC = 0, ALNZ_RSC = 0, ALAL_RSC = 0, ALFZG_MSC = 0, ALNZ_MSC = 0, ALAL_MSC = 0)



for (m in c("BTC", "ETH", "LTC", "XRP")){
  losses1 = VaRES %>% filter(moeda == m, classe == "Indiv.", how_many_ct==1) %>% select(methods)
  losses1 = losses1$methods
  losses2 = VaRES %>% filter(moeda == m, classe == "Comb.", how_many_ct==1) %>% select(methods)
  losses2 = losses2$methods
  if (m == "BTC"){ VaR_ = VaRBTC; ret_ = retBTC; ES_ = ESBTC}
  if (m == "ETH"){ VaR_ = VaRETH; ret_ = retETH; ES_ = ESETH}
  if (m == "LTC"){ VaR_ = VaRLTC; ret_ = retLTC; ES_ = ESLTC}
  if (m == "XRP"){ VaR_ = VaRXRP; ret_ = retXRP; ES_ = ESXRP}
  for (i in losses1){
    for (j in losses2){
      GR_QL <- fluct_test(QL(VaR_[,i], ret_, alpha = a), QL(VaR_[,j], ret_, alpha = a),
                          mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
      GR_FZG <- fluct_test(FZG(VaR_[,i], ES_[,i], ret_, alpha = a), FZG(VaR_[,j], ES_[,j], ret_, alpha = a),
                               mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
      GR_NZ <- fluct_test(NZ(VaR_[,i], ES_[,i], ret_, alpha = a), NZ(VaR_[,j], ES_[,j], ret_, alpha = a),
                             mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
      GR_AL <- fluct_test(AL(VaR_[,i], ES_[,i], ret_, alpha = a), AL(VaR_[,j], ES_[,j], ret_, alpha = a),
                            mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)

      VaRES[which(VaRES$moeda == m & VaRES$classe == "Comb." & VaRES$methods == j),VaRES %>% select(ends_with(i)) %>% colnames()] = c(ifelse(max(GR_QL$fluc$y)> GR_QL$cv_sup, 1, 0),
                                                                                                                                      ifelse(max(GR_FZG$fluc$y)> GR_FZG$cv_sup, 1, 0),
                                                                                                                                      ifelse(max(GR_NZ$fluc$y)> GR_NZ$cv_sup, 1, 0),
                                                                                                                                      ifelse(max(GR_AL$fluc$y)> GR_AL$cv_sup, 1, 0))
    }
  }
}
   
   
   
for (m in c("BTC", "ETH", "LTC", "XRP")){
     losses1 = VaRES %>% filter(moeda == m, classe == "Comb.", how_many_ct==1) %>% select(methods)
     losses1 = losses1$methods
     losses2 = VaRES %>% filter(moeda == m, classe == "Indiv.", how_many_ct==1) %>% select(methods)
     losses2 = losses2$methods
     if (m == "BTC"){ VaR_ = VaRBTC; ret_ = retBTC; ES_ = ESBTC}
     if (m == "ETH"){ VaR_ = VaRETH; ret_ = retETH; ES_ = ESETH}
     if (m == "LTC"){ VaR_ = VaRLTC; ret_ = retLTC; ES_ = ESLTC}
     if (m == "XRP"){ VaR_ = VaRXRP; ret_ = retXRP; ES_ = ESXRP}
     for (i in losses1){
       for (j in losses2){
         GR_QL <- fluct_test(QL(VaR_[,i], ret_, alpha = a), QL(VaR_[,j], ret_, alpha = a),
                             mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
         GR_FZG <- fluct_test(FZG(VaR_[,i], ES_[,i], ret_, alpha = a), FZG(VaR_[,j], ES_[,j], ret_, alpha = a),
                              mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
         GR_NZ <- fluct_test(NZ(VaR_[,i], ES_[,i], ret_, alpha = a), NZ(VaR_[,j], ES_[,j], ret_, alpha = a),
                             mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
         GR_AL <- fluct_test(AL(VaR_[,i], ES_[,i], ret_, alpha = a), AL(VaR_[,j], ES_[,j], ret_, alpha = a),
                             mu = mu_, lag_truncate = 5, alpha = 0.1, dmv_fullsample = TRUE)
         
         VaRES[which(VaRES$moeda == m & VaRES$classe == "Indiv." & VaRES$methods == j),VaRES %>% select(ends_with(i)) %>% colnames()] = c(ifelse(max(GR_QL$fluc$y)> GR_QL$cv_sup, 1, 0),
                                                                                                                                         ifelse(max(GR_FZG$fluc$y)> GR_FZG$cv_sup, 1, 0),
                                                                                                                                         ifelse(max(GR_NZ$fluc$y)> GR_NZ$cv_sup, 1, 0),
                                                                                                                                         ifelse(max(GR_AL$fluc$y)> GR_AL$cv_sup, 1, 0))
       }
     }
   }   
   


VaRES = VaRES %>%
  mutate(AQL = paste0(format(round(VaRES$AQL,4),nsmall =4),ifelse(VaRES$QLGAS==1, "$^G$", ""),
                      ifelse(VaRES$QLMSGARCH==1, "$^M$", ""),
                      ifelse(VaRES$QLBoot.==1, "$^B$", ""),
                      ifelse(VaRES$QLFIGARCH==1, "$^F$", ""),
                      ifelse(VaRES$QLAVGARCH==1, "$^A$", ""),
                      ifelse(VaRES$QLAVG==1, "$^1$", ""),
                      ifelse(VaRES$QLMED==1, "$^2$", ""),
                      ifelse(VaRES$QLFZG_RSC==1, "$^3$", ""),
                      ifelse(VaRES$QLNZ_RSC==1, "$^4$", ""),
                      ifelse(VaRES$QLAL_RSC==1, "$^5$", ""),
                      ifelse(VaRES$QLFZG_MSC==1, "$^6$", ""),
                      ifelse(VaRES$QLNZ_MSC==1, "$^7$", ""),
                      ifelse(VaRES$QLAL_MSC==1, "$^8$", "")),
       AFZG = paste0(format(round(VaRES$AFZG,4),nsmall =4),ifelse(VaRES$FZGGAS==1, "$^G$", ""),
                     ifelse(VaRES$FZGMSGARCH==1, "$^M$", ""),
                     ifelse(VaRES$FZGBoot.==1, "$^B$", ""),
                     ifelse(VaRES$FZGFIGARCH==1, "$^F$", ""),
                     ifelse(VaRES$FZGAVGARCH==1, "$^A$", ""),
                     ifelse(VaRES$FZGAVG==1, "$^1$", ""),
                     ifelse(VaRES$FZGMED==1, "$^2$", ""),
                     ifelse(VaRES$FZGFZG_RSC==1, "$^3$", ""),
                     ifelse(VaRES$FZGNZ_RSC==1, "$^4$", ""),
                     ifelse(VaRES$FZGAL_RSC==1, "$^5$", ""),
                     ifelse(VaRES$FZGFZG_MSC==1, "$^6$", ""),
                     ifelse(VaRES$FZGNZ_MSC==1, "$^7$", ""),
                     ifelse(VaRES$FZGAL_MSC==1, "$^8$", "")),
       ANZ = paste0(format(round(VaRES$ANZ,4),nsmall =4),ifelse(VaRES$NZGAS==1, "$^G$", ""),
                    ifelse(VaRES$NZMSGARCH==1, "$^M$", ""),
                    ifelse(VaRES$NZBoot.==1, "$^B$", ""),
                    ifelse(VaRES$NZFIGARCH==1, "$^F$", ""),
                    ifelse(VaRES$NZAVGARCH==1, "$^A$", ""),
                    ifelse(VaRES$NZAVG==1, "$^1$", ""),
                    ifelse(VaRES$NZMED==1, "$^2$", ""),
                    ifelse(VaRES$NZFZG_RSC==1, "$^3$", ""),
                    ifelse(VaRES$NZNZ_RSC==1, "$^4$", ""),
                    ifelse(VaRES$NZAL_RSC==1, "$^5$", ""),
                    ifelse(VaRES$NZFZG_MSC==1, "$^6$", ""),
                    ifelse(VaRES$NZNZ_MSC==1, "$^7$", ""),
                    ifelse(VaRES$NZAL_MSC==1, "$^8$", "")),
       AAL = paste0(format(round(VaRES$AAL,4),nsmall =4),ifelse(VaRES$ALGAS==1, "$^G$", ""),
                    ifelse(VaRES$ALMSGARCH==1, "$^M$", ""),
                    ifelse(VaRES$ALBoot.==1, "$^B$", ""),
                    ifelse(VaRES$ALFIGARCH==1, "$^F$", ""),
                    ifelse(VaRES$ALAVGARCH==1, "$^A$", ""),
                    ifelse(VaRES$ALAVG==1, "$^1$", ""),
                    ifelse(VaRES$ALMED==1, "$^2$", ""),
                    ifelse(VaRES$ALFZG_RSC==1, "$^3$", ""),
                    ifelse(VaRES$ALNZ_RSC==1, "$^4$", ""),
                    ifelse(VaRES$ALAL_RSC==1, "$^5$", ""),
                    ifelse(VaRES$ALFZG_MSC==1, "$^6$", ""),
                    ifelse(VaRES$ALNZ_MSC==1, "$^7$", ""),
                    ifelse(VaRES$ALAL_MSC==1, "$^8$", "")))

#############################################
###       Latex Table format              ###
#############################################

setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/WP_2021")
VaRES %>% 
  mutate(method = rep(c("GAS", "MSGARCH", "Boot.", "FIGARCH", "AVGARCH",
                                "AVG", "MED","$\\rm{RSC_{FZG}}$","$\\rm{RSC_{NZ}}$","$\\rm{RSC_{AL}}$",
                                "$\\rm{MSC_{FZG}}$","$\\rm{MSC_{NZ}}$","$\\rm{MSC_{AL}}$"),4)) %>% 
  select(moeda, classe, method, Hits, CC, DQ, VQ,MFE,NZ,ESR_1, ESR_2, ESR_3,AQL,AFZG,ANZ,AAL) %>% 
  kbl(format = "latex", digits = c(1,1,1,1,3,3,3,3,3,3,3,3,4,4,4,4), booktabs = T,
      row.names = FALSE, escape = FALSE, caption = Caption, label = label_name,
      col.names = c("","","","Hits", "CC", "DQ","VQ", "ER","CoC", "ESR1", "ESR2", "ESR3", "QL", "FZG","NZ", "AL")) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle",row_group_label_position = "identity") %>% 
  add_header_above(c("", " ", " ", " ", "Calibration tests" = 8, "Average Scoring functions" = 4)) %>% 
  #column_spec(11, bold = ifelse(VaRES$how_many_ct == 1, T, F)) %>% 
  #column_spec(12, bold = ifelse(VaRES$how_many_ct == 1, T, F)) %>% 
  #column_spec(13, bold = ifelse(VaRES$how_many_ct == 1, T, F)) %>% 
  #column_spec(14, bold = ifelse(VaRES$how_many_ct == 1, T, F)) %>% 
  #column_spec(5, background = ifelse(VaRES$CC > p, "gray!25", "white")) %>% 
  #column_spec(6, background = ifelse(VaRES$DQ > p, "gray!25", "white")) %>% 
  #column_spec(7, background = ifelse(VaRES$VQ > p, "gray!25", "white")) %>% 
  #column_spec(9, background = ifelse(VaRES$NZ > p, "gray!25", "white")) %>% 
  #column_spec(8, background = ifelse(VaRES$MFE > p, "gray!25", "white")) %>% 
  #column_spec(10, background = ifelse(VaRES$ESR_3 > p, "gray!25", "white")) %>% 
  row_spec(which(VaRES$how_many_ct == 1), background = "gray!25") %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 7) %>% 
  save_kable(keep_tex = T, file = file_tex_name)




  


