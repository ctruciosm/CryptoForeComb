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
  
  VaR_JER = read.csv("VaR_JER.csv",sep = " ")
  ES_JER  = read.csv("ES_JER.csv",sep = " ")  
  
  VaR_full = VaR %>% data.frame() %>% 
    select(ends_with(risklevel)) %>%
    mutate(FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
           FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl], JER = VaR_JER[,rl])
  
  ES_full = ES %>% data.frame() %>% 
    select(ends_with(risklevel)) %>%
    mutate(FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
           FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl], JER = ES_JER[,rl])
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
library(Rcpp)
library(quantreg)
sourceCpp("scoring_functions.cpp")
source("Function_VaR_VQR.R")
source("esr_backtest_modified.R")
source("Optimizations.R")
source("00_GiacominiRossi.R")


p = 0.05
pMCS = 0.05
mu_ = 0.1
end_date = "2021-08-14"
MCS_type = "t.range"
block_length =  12

a2 = 0.025; a5 = 0.050; a10 = 0.1;
Caption_btc = "One-step-ahead VaR and ES backtesting for BTC at 2.5\\%, 5\\% and 10\\%  risk levels. Shaded rows indicate procedures with p-values larger than 0.05 in all calibration tests."
file_tex_name_btc = "VaRES_btc.tex" 
label_name_btc = "Table_VaRES_btc"

Caption_eth = "One-step-ahead VaR and ES backtesting for ETH at 2.5\\%, 5\\% and 10\\%  risk levels. Shaded rows indicate procedures with p-values larger than 0.05 in all calibration tests."
file_tex_name_eth = "VaRES_eth.tex" 
label_name_eth = "Table_VaRES_eth"

setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/BTC/")
if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  crypto = read.csv("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(as.character(2), 1)
  retBTC = AUX[[1]]
  VaRBTC2 = AUX[[2]]
  ESBTC2  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(5), 2)
  VaRBTC5 = AUX[[2]]
  ESBTC5  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(10), 3)
  VaRBTC10 = AUX[[2]]
  ESBTC10  = AUX[[3]]
}
setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/ETH/")
if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  crypto = read.csv("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
  OoS = 600
  InS = dim(crypto)[1]-OoS
  crypto = crypto[(InS+1):(InS+OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(as.character(2), 1)
  retETH = AUX[[1]]
  VaRETH2 = AUX[[2]]
  ESETH2  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(5), 2)
  VaRETH5 = AUX[[2]]
  ESETH5  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(10), 3)
  VaRETH10 = AUX[[2]]
  ESETH10  = AUX[[3]]
}



###########################################
# Including Basic Combining Strategies
### Mean
VaRBTC2$AVG = apply(VaRBTC2[,1:n_ind],1,mean)
VaRETH2$AVG = apply(VaRETH2[,1:n_ind],1,mean)
ESBTC2$AVG = apply(ESBTC2[,1:n_ind],1,mean)
ESETH2$AVG = apply(ESETH2[,1:n_ind],1,mean)

VaRBTC5$AVG = apply(VaRBTC5[,1:n_ind],1,mean)
VaRETH5$AVG = apply(VaRETH5[,1:n_ind],1,mean)
ESBTC5$AVG = apply(ESBTC5[,1:n_ind],1,mean)
ESETH5$AVG = apply(ESETH5[,1:n_ind],1,mean)

VaRBTC10$AVG = apply(VaRBTC10[,1:n_ind],1,mean)
VaRETH10$AVG = apply(VaRETH10[,1:n_ind],1,mean)
ESBTC10$AVG = apply(ESBTC10[,1:n_ind],1,mean)
ESETH10$AVG = apply(ESETH10[,1:n_ind],1,mean)


### Median
VaRBTC2$MED = apply(VaRBTC2[,1:n_ind],1,median)
VaRETH2$MED = apply(VaRETH2[,1:n_ind],1,median)
ESBTC2$MED = apply(ESBTC2[,1:n_ind],1,median)
ESETH2$MED = apply(ESETH2[,1:n_ind],1,median)

VaRBTC5$MED = apply(VaRBTC5[,1:n_ind],1,median)
VaRETH5$MED = apply(VaRETH5[,1:n_ind],1,median)
ESBTC5$MED = apply(ESBTC5[,1:n_ind],1,median)
ESETH5$MED = apply(ESETH5[,1:n_ind],1,median)

VaRBTC10$MED = apply(VaRBTC10[,1:n_ind],1,median)
VaRETH10$MED = apply(VaRETH10[,1:n_ind],1,median)
ESBTC10$MED = apply(ESBTC10[,1:n_ind],1,median)
ESETH10$MED = apply(ESETH10[,1:n_ind],1,median)


### Min
VaRBTC2$MIN = apply(VaRBTC2[,1:n_ind],1,min)
VaRETH2$MIN = apply(VaRETH2[,1:n_ind],1,min)
ESBTC2$MIN = apply(ESBTC2[,1:n_ind],1,min)
ESETH2$MIN = apply(ESETH2[,1:n_ind],1,min)

VaRBTC5$MIN = apply(VaRBTC5[,1:n_ind],1,min)
VaRETH5$MIN = apply(VaRETH5[,1:n_ind],1,min)
ESBTC5$MIN = apply(ESBTC5[,1:n_ind],1,min)
ESETH5$MIN = apply(ESETH5[,1:n_ind],1,min)

VaRBTC10$MIN = apply(VaRBTC10[,1:n_ind],1,min)
VaRETH10$MIN = apply(VaRETH10[,1:n_ind],1,min)
ESBTC10$MIN = apply(ESBTC10[,1:n_ind],1,min)
ESETH10$MIN = apply(ESETH10[,1:n_ind],1,min)


### Max
VaRBTC2$MAX = apply(VaRBTC2[,1:n_ind],1,max)
VaRETH2$MAX = apply(VaRETH2[,1:n_ind],1,max)
ESBTC2$MAX = apply(ESBTC2[,1:n_ind],1,max)
ESETH2$MAX = apply(ESETH2[,1:n_ind],1,max)

VaRBTC5$MAX = apply(VaRBTC5[,1:n_ind],1,max)
VaRETH5$MAX = apply(VaRETH5[,1:n_ind],1,max)
ESBTC5$MAX = apply(ESBTC5[,1:n_ind],1,max)
ESETH5$MAX = apply(ESETH5[,1:n_ind],1,max)

VaRBTC10$MAX = apply(VaRBTC10[,1:n_ind],1,max)
VaRETH10$MAX = apply(VaRETH10[,1:n_ind],1,max)
ESBTC10$MAX = apply(ESBTC10[,1:n_ind],1,max)
ESETH10$MAX = apply(ESETH10[,1:n_ind],1,max)


###########################################
# Reordering

ordering2 <- c(paste0("GAS","2"),paste0("MSGARCH","2"),paste0("Boot","2"),
              paste0("FIGARCH","2"),paste0("CSGARCH","2"),
              "AVG","MED","MIN","MAX",
              "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

ordering5 <- c(paste0("GAS","5"),paste0("MSGARCH","5"),paste0("Boot","5"),
               paste0("FIGARCH","5"),paste0("CSGARCH","5"),
               "AVG","MED","MIN","MAX",
               "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

ordering10 <- c(paste0("GAS","10"),paste0("MSGARCH","10"),paste0("Boot","10"),
               paste0("FIGARCH","10"),paste0("CSGARCH","10"),
               "AVG","MED","MIN","MAX",
               "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

names = c("GAS", "MSGARCH", "Boot.", "FIGARCH", "CGARCH",
          "AVG", "MED", "MIN", "MAX", 
          "FZG_RSC","NZ_RSC","AL_RSC","FZG_MSC","NZ_MSC", "AL_MSC")

VaRBTC2 <- VaRBTC2 %>% select(ordering2)
colnames(VaRBTC2) = names
VaRETH2 <- VaRETH2 %>% select(ordering2)
colnames(VaRETH2) = names

VaRBTC5 <- VaRBTC5 %>% select(ordering5)
colnames(VaRBTC5) = names
VaRETH5 <- VaRETH5 %>% select(ordering5)
colnames(VaRETH5) = names

VaRBTC10 <- VaRBTC10 %>% select(ordering10)
colnames(VaRBTC10) = names
VaRETH10 <- VaRETH10 %>% select(ordering10)
colnames(VaRETH10) = names


ESBTC2 <- ESBTC2 %>% select(ordering2)
colnames(ESBTC2) = names
ESETH2 <- ESETH2 %>% select(ordering2)
colnames(ESETH2) = names

ESBTC5 <- ESBTC5 %>% select(ordering5)
colnames(ESBTC5) = names
ESETH5 <- ESETH5 %>% select(ordering5)
colnames(ESETH5) = names

ESBTC10 <- ESBTC10 %>% select(ordering10)
colnames(ESBTC10) = names
ESETH10 <- ESETH10 %>% select(ordering10)
colnames(ESETH10) = names




###########################################
#########   VaR Backtesting      ##########
###########################################
K = dim(VaRETH2)[2]  

BackVaRESBTC2 = BackVaRESETH2 = BackVaRESBTC5 = BackVaRESETH5 = BackVaRESBTC10 = BackVaRESETH10 = matrix(0,ncol = 18,nrow = K) 
colnames(BackVaRESBTC2) = colnames(BackVaRESETH2) = colnames(BackVaRESBTC5) = colnames(BackVaRESETH5) = colnames(BackVaRESBTC10) = colnames(BackVaRESETH10) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESR_1", "ESR_2" ,"ESR_3", "AQL", "AFZG", "ANZ", "AAL","AQL2", "AFZG2", "ANZ2", "AAL2")

for (i in 1:K){ # each i is a method (individual or combination)
  set.seed(1234)
  BackTBTC2 = BacktestVaR(retBTC, VaRBTC2[,i], alpha = a2, Lags = 4)
  BackTBTC5 = BacktestVaR(retBTC, VaRBTC5[,i], alpha = a5, Lags = 4)
  BackTBTC10= BacktestVaR(retBTC, VaRBTC10[,i],alpha = a10,Lags = 4)
  BackTETH2 = BacktestVaR(retETH, VaRETH2[,i], alpha = a2, Lags = 4)
  BackTETH5 = BacktestVaR(retETH, VaRETH5[,i], alpha = a5, Lags = 4)
  BackTETH10= BacktestVaR(retETH, VaRETH10[,i],alpha = a10,Lags = 4)

  EBackTBTC2 = ESTest(alpha = a2, retBTC, ESBTC2[,i], VaRBTC2[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH2 = ESTest(alpha = a2, retETH, ESETH2[,i], VaRETH2[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTBTC5 = ESTest(alpha = a5, retBTC, ESBTC5[,i], VaRBTC5[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH5 = ESTest(alpha = a5, retETH, ESETH5[,i], VaRETH5[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTBTC10= ESTest(alpha = a10, retBTC, ESBTC10[,i], VaRBTC10[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH10= ESTest(alpha = a10, retETH, ESETH10[,i], VaRETH10[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  
  
  BackVaRESBTC2[i,] = c(mean(retBTC < VaRBTC2[,i])*100, 
                       BackTBTC2$LRuc[2], BackTBTC2$LRcc[2],BackTBTC2$DQ$pvalue, VaR_VQR(retBTC, VaRBTC2[,i], a2),
                       EBackTBTC2$boot.p.value,
                       cc_backtest(retBTC, VaRBTC2[,i], ESBTC2[,i],  alpha  = a2)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 3)$pvalue_onesided_asymptotic),
                       mean(QL(matrix(VaRBTC2[,i], ncol = 1) ,retBTC, alpha = a2)),
                       mean(FZG(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)),
                       mean(NZ(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)),
                       mean(AL(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)),
                       mean(QL(matrix(VaRBTC2[-CovidDay,i], ncol = 1),retBTC[-CovidDay], alpha = a2)),
                       mean(FZG(matrix(VaRBTC2[-CovidDay,i], ncol = 1), matrix(ESBTC2[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a2)),
                       mean(NZ(matrix(VaRBTC2[-CovidDay,i], ncol = 1), matrix(ESBTC2[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a2)),
                       mean(AL(matrix(VaRBTC2[-CovidDay,i], ncol = 1), matrix(ESBTC2[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a2)))
  
  BackVaRESBTC5[i,] = c(mean(retBTC < VaRBTC5[,i])*100, 
                        BackTBTC5$LRuc[2], BackTBTC5$LRcc[2],BackTBTC5$DQ$pvalue, VaR_VQR(retBTC, VaRBTC5[,i], a5),
                        EBackTBTC5$boot.p.value,
                        cc_backtest(retBTC, VaRBTC5[,i], ESBTC5[,i],  alpha  = a5)$pvalue_twosided_simple, 
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 3)$pvalue_onesided_asymptotic),
                        mean(QL(matrix(VaRBTC5[,i], ncol = 1) ,retBTC, alpha = a5)),
                        mean(FZG(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)),
                        mean(NZ(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)),
                        mean(AL(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)),
                        mean(QL(matrix(VaRBTC5[-CovidDay,i], ncol = 1),retBTC[-CovidDay], alpha = a5)),
                        mean(FZG(matrix(VaRBTC5[-CovidDay,i], ncol = 1), matrix(ESBTC5[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a5)),
                        mean(NZ(matrix(VaRBTC5[-CovidDay,i], ncol = 1), matrix(ESBTC5[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a5)),
                        mean(AL(matrix(VaRBTC5[-CovidDay,i], ncol = 1), matrix(ESBTC5[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a5)))
  
  BackVaRESBTC10[i,]= c(mean(retBTC < VaRBTC10[,i])*100, 
                        BackTBTC10$LRuc[2], BackTBTC10$LRcc[2],BackTBTC10$DQ$pvalue, VaR_VQR(retBTC, VaRBTC10[,i], a10),
                        EBackTBTC10$boot.p.value,
                        cc_backtest(retBTC, VaRBTC10[,i], ESBTC10[,i],  alpha  = a10)$pvalue_twosided_simple, 
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC10[,i], ESBTC10[,i],alpha  = a10, B = 0, version = 1)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC10[,i], ESBTC10[,i],alpha  = a10, B = 0, version = 2)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retBTC, VaRBTC10[,i], ESBTC10[,i],alpha  = a10, B = 0, version = 3)$pvalue_onesided_asymptotic),
                        mean(QL(matrix(VaRBTC10[,i], ncol = 1) ,retBTC, alpha = a10)),
                        mean(FZG(matrix(VaRBTC10[,i], ncol = 1), matrix(ESBTC10[,i], ncol = 1), retBTC, alpha = a10)),
                        mean(NZ(matrix(VaRBTC10[,i], ncol = 1), matrix(ESBTC10[,i], ncol = 1), retBTC, alpha = a10)),
                        mean(AL(matrix(VaRBTC10[,i], ncol = 1), matrix(ESBTC10[,i], ncol = 1), retBTC, alpha = a10)),
                        mean(QL(matrix(VaRBTC10[-CovidDay,i], ncol = 1),retBTC[-CovidDay], alpha = a10)),
                        mean(FZG(matrix(VaRBTC10[-CovidDay,i], ncol = 1), matrix(ESBTC10[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a10)),
                        mean(NZ(matrix(VaRBTC10[-CovidDay,i], ncol = 1), matrix(ESBTC10[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a10)),
                        mean(AL(matrix(VaRBTC10[-CovidDay,i], ncol = 1), matrix(ESBTC10[-CovidDay,i], ncol = 1), retBTC[-CovidDay], alpha = a10)))
  
  
  BackVaRESETH2[i,] = c(mean(retETH < VaRETH2[,i])*100, 
                       BackTETH2$LRuc[2], BackTETH2$LRcc[2],BackTETH2$DQ$pvalue, VaR_VQR(retETH, VaRETH2[,i], a2),
                       EBackTETH2$boot.p.value,
                       cc_backtest(retETH, VaRETH2[,i], ESETH2[,i],  alpha  = a2)$pvalue_twosided_simple, 
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic),
                       suppressWarnings(esr_backtest_modified(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 3)$pvalue_onesided_asymptotic),
                       mean(QL(matrix(VaRETH2[,i], ncol = 1),retETH, alpha = a2)),
                       mean(FZG(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)),
                       mean(NZ(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)),
                       mean(AL(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)),
                       mean(QL(matrix(VaRETH2[-CovidDay,i], ncol = 1),retETH[-CovidDay], alpha = a2)),
                       mean(FZG(matrix(VaRETH2[-CovidDay,i], ncol = 1), matrix(ESETH2[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a2)),
                       mean(NZ(matrix(VaRETH2[-CovidDay,i], ncol = 1), matrix(ESETH2[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a2)),
                       mean(AL(matrix(VaRETH2[-CovidDay,i], ncol = 1), matrix(ESETH2[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a2)))

  BackVaRESETH5[i,] = c(mean(retETH < VaRETH5[,i])*100, 
                        BackTETH5$LRuc[2], BackTETH5$LRcc[2],BackTETH5$DQ$pvalue, VaR_VQR(retETH, VaRETH5[,i], a5),
                        EBackTETH5$boot.p.value,
                        cc_backtest(retETH, VaRETH5[,i], ESETH5[,i],  alpha  = a5)$pvalue_twosided_simple, 
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 3)$pvalue_onesided_asymptotic),
                        mean(QL(matrix(VaRETH5[,i], ncol = 1),retETH, alpha = a5)),
                        mean(FZG(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)),
                        mean(NZ(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)),
                        mean(AL(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)),
                        mean(QL(matrix(VaRETH5[-CovidDay,i], ncol = 1),retETH[-CovidDay], alpha = a5)),
                        mean(FZG(matrix(VaRETH5[-CovidDay,i], ncol = 1), matrix(ESETH5[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a5)),
                        mean(NZ(matrix(VaRETH5[-CovidDay,i], ncol = 1), matrix(ESETH5[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a5)),
                        mean(AL(matrix(VaRETH5[-CovidDay,i], ncol = 1), matrix(ESETH5[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a5)))
  
  BackVaRESETH10[i,] = c(mean(retETH < VaRETH10[,i])*100, 
                        BackTETH10$LRuc[2], BackTETH10$LRcc[2],BackTETH10$DQ$pvalue, VaR_VQR(retETH, VaRETH10[,i], a10),
                        EBackTETH10$boot.p.value,
                        cc_backtest(retETH, VaRETH10[,i], ESETH10[,i],  alpha  = a10)$pvalue_twosided_simple, 
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH10[,i], ESETH10[,i],alpha  = a10, B = 0, version = 1)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH10[,i], ESETH10[,i],alpha  = a10, B = 0, version = 2)$pvalue_twosided_asymptotic),
                        suppressWarnings(esr_backtest_modified(retETH, VaRETH10[,i], ESETH10[,i],alpha  = a10, B = 0, version = 3)$pvalue_onesided_asymptotic),
                        mean(QL(matrix(VaRETH10[,i], ncol = 1),retETH, alpha = a10)),
                        mean(FZG(matrix(VaRETH10[,i], ncol = 1), matrix(ESETH10[,i], ncol = 1), retETH, alpha = a10)),
                        mean(NZ(matrix(VaRETH10[,i], ncol = 1), matrix(ESETH10[,i], ncol = 1), retETH, alpha = a10)),
                        mean(AL(matrix(VaRETH10[,i], ncol = 1), matrix(ESETH10[,i], ncol = 1), retETH, alpha = a10)),
                        mean(QL(matrix(VaRETH10[-CovidDay,i], ncol = 1),retETH[-CovidDay], alpha = a10)),
                        mean(FZG(matrix(VaRETH10[-CovidDay,i], ncol = 1), matrix(ESETH10[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a10)),
                        mean(NZ(matrix(VaRETH10[-CovidDay,i], ncol = 1), matrix(ESETH10[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a10)),
                        mean(AL(matrix(VaRETH10[-CovidDay,i], ncol = 1), matrix(ESETH10[-CovidDay,i], ncol = 1), retETH[-CovidDay], alpha = a10)))

}


###########################################
#######    Model Confidence Set      ######
###########################################

MCSBTC_MQL2 = rep(0,ncol(VaRBTC2))
MQL2 = QL(as.matrix(VaRBTC2),retBTC, alpha = a2)
colnames(MQL2) = colnames(VaRBTC2)
auxBTC_MQL2 = estMCS.quick(MQL2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MQL2[auxBTC_MQL2] = 1

MCSBTC_MQL5 = rep(0,ncol(VaRBTC5))
MQL5 = QL(as.matrix(VaRBTC5),retBTC, alpha = a5)
colnames(MQL5) = colnames(VaRBTC5)
auxBTC_MQL5 = estMCS.quick(MQL5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MQL5[auxBTC_MQL5] = 1

MCSBTC_MQL10 = rep(0,ncol(VaRBTC10))
MQL10 = QL(as.matrix(VaRBTC10),retBTC, alpha = a10)
colnames(MQL10) = colnames(VaRBTC10)
auxBTC_MQL10 = estMCS.quick(MQL10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MQL10[auxBTC_MQL10] = 1


MCSBTC_MFZG2 = rep(0,ncol(VaRBTC2))
MFZG2 = FZG(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MFZG2) = colnames(VaRBTC2)
auxBTC_MFZG2 = estMCS.quick(MFZG2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MFZG2[auxBTC_MFZG2] = 1

MCSBTC_MFZG5 = rep(0,ncol(VaRBTC5))
MFZG5 = FZG(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MFZG5) = colnames(VaRBTC5)
auxBTC_MFZG5 = estMCS.quick(MFZG5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MFZG5[auxBTC_MFZG5] = 1

MCSBTC_MFZG10 = rep(0,ncol(VaRBTC10))
MFZG10 = FZG(as.matrix(VaRBTC10), as.matrix(ESBTC10), retBTC, alpha = a10)
colnames(MFZG10) = colnames(VaRBTC10)
auxBTC_MFZG10 = estMCS.quick(MFZG10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MFZG10[auxBTC_MFZG10] = 1

 
MCSBTC_MNZ2 = rep(0,ncol(VaRBTC2))
MNZ2 = NZ(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MNZ2) = colnames(VaRBTC2)
auxBTC_MNZ2 = estMCS.quick(MNZ2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MNZ2[auxBTC_MNZ2] = 1

MCSBTC_MNZ5 = rep(0,ncol(VaRBTC5))
MNZ5 = NZ(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MNZ5) = colnames(VaRBTC5)
auxBTC_MNZ5 = estMCS.quick(MNZ5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MNZ5[auxBTC_MNZ5] = 1

MCSBTC_MNZ10 = rep(0,ncol(VaRBTC10))
MNZ10 = NZ(as.matrix(VaRBTC10), as.matrix(ESBTC10), retBTC, alpha = a10)
colnames(MNZ10) = colnames(VaRBTC10)
auxBTC_MNZ10 = estMCS.quick(MNZ10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MNZ10[auxBTC_MNZ10] = 1


MCSBTC_MAL2 = rep(0,ncol(VaRBTC2))
MAL2 = AL(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MAL2) = colnames(VaRBTC2)
auxBTC_MAL2 = estMCS.quick(MAL2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MAL2[auxBTC_MAL2] = 1

MCSBTC_MAL5 = rep(0,ncol(VaRBTC5))
MAL5 = AL(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MAL5) = colnames(VaRBTC5)
auxBTC_MAL5 = estMCS.quick(MAL5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MAL5[auxBTC_MAL5] = 1

MCSBTC_MAL10 = rep(0,ncol(VaRBTC10))
MAL10 = AL(as.matrix(VaRBTC10), as.matrix(ESBTC10), retBTC, alpha = a10)
colnames(MAL10) = colnames(VaRBTC10)
auxBTC_MAL10 = estMCS.quick(MAL10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSBTC_MAL10[auxBTC_MAL10] = 1



MCSETH_MQL2 = rep(0,ncol(VaRETH2))
MQL2 = QL(as.matrix(VaRETH2),retETH, alpha = a2)
colnames(MQL2) = colnames(VaRETH2)
auxETH_MQL2 = estMCS.quick(MQL2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MQL2[auxETH_MQL2] = 1

MCSETH_MQL5 = rep(0,ncol(VaRETH5))
MQL5 = QL(as.matrix(VaRETH5),retETH, alpha = a5)
colnames(MQL5) = colnames(VaRETH5)
auxETH_MQL5 = estMCS.quick(MQL5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MQL5[auxETH_MQL5] = 1

MCSETH_MQL10 = rep(0,ncol(VaRETH10))
MQL10 = QL(as.matrix(VaRETH10),retETH, alpha = a10)
colnames(MQL10) = colnames(VaRETH10)
auxETH_MQL10 = estMCS.quick(MQL10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MQL10[auxETH_MQL10] = 1


MCSETH_MFZG2 = rep(0,ncol(VaRETH2))
MFZG2 = FZG(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MFZG2) = colnames(VaRETH2)
auxETH_MFZG2 = estMCS.quick(MFZG2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MFZG2[auxETH_MFZG2] = 1

MCSETH_MFZG5 = rep(0,ncol(VaRETH5))
MFZG5 = FZG(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MFZG5) = colnames(VaRETH5)
auxETH_MFZG5 = estMCS.quick(MFZG5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MFZG5[auxETH_MFZG5] = 1

MCSETH_MFZG10 = rep(0,ncol(VaRETH10))
MFZG10 = FZG(as.matrix(VaRETH10), as.matrix(ESETH10), retETH, alpha = a10)
colnames(MFZG10) = colnames(VaRETH10)
auxETH_MFZG10 = estMCS.quick(MFZG10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MFZG10[auxETH_MFZG10] = 1


MCSETH_MNZ2 = rep(0,ncol(VaRETH2))
MNZ2 = NZ(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MNZ2) = colnames(VaRETH2)
auxETH_MNZ2 = estMCS.quick(MNZ2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MNZ2[auxETH_MNZ2] = 1

MCSETH_MNZ5 = rep(0,ncol(VaRETH5))
MNZ5 = NZ(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MNZ5) = colnames(VaRETH5)
auxETH_MNZ5 = estMCS.quick(MNZ5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MNZ5[auxETH_MNZ5] = 1

MCSETH_MNZ10 = rep(0,ncol(VaRETH10))
MNZ10 = NZ(as.matrix(VaRETH10), as.matrix(ESETH10), retETH, alpha = a10)
colnames(MNZ10) = colnames(VaRETH10)
auxETH_MNZ10 = estMCS.quick(MNZ10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MNZ10[auxETH_MNZ10] = 1


MCSETH_MAL2 = rep(0,ncol(VaRETH2))
MAL2 = AL(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MAL2) = colnames(VaRETH2)
auxETH_MAL2 = estMCS.quick(MAL2, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MAL2[auxETH_MAL2] = 1

MCSETH_MAL5 = rep(0,ncol(VaRETH5))
MAL5 = AL(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MAL5) = colnames(VaRETH5)
auxETH_MAL5 = estMCS.quick(MAL5, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MAL5[auxETH_MAL5] = 1

MCSETH_MAL10 = rep(0,ncol(VaRETH10))
MAL10 = AL(as.matrix(VaRETH10), as.matrix(ESETH10), retETH, alpha = a10)
colnames(MAL10) = colnames(VaRETH10)
auxETH_MAL10 = estMCS.quick(MAL10, test=MCS_type, B=5000, l=block_length, alpha = pMCS)
MCSETH_MAL10[auxETH_MAL10] = 1

 
 
MCCQL_BTC = c(MCSBTC_MQL2,MCSBTC_MQL5,MCSBTC_MQL10)
MCCFZG_BTC = c(MCSBTC_MFZG2,MCSBTC_MFZG5,MCSBTC_MFZG10)
MCCNZ_BTC = c(MCSBTC_MNZ2,MCSBTC_MNZ5,MCSBTC_MNZ10)
MCCAL_BTC = c(MCSBTC_MAL2,MCSBTC_MAL5,MCSBTC_MAL10)

MCCQL_ETH = c(MCSETH_MQL2,MCSETH_MQL5,MCSETH_MQL10)
MCCFZG_ETH = c(MCSETH_MFZG2,MCSETH_MFZG5,MCSETH_MFZG10)
MCCNZ_ETH = c(MCSETH_MNZ2,MCSETH_MNZ5,MCSETH_MNZ10)
MCCAL_ETH = c(MCSETH_MAL2,MCSETH_MAL5,MCSETH_MAL10)


#############################################
###       Latex Table format              ###
#############################################

VaRESBTC = rbind(BackVaRESBTC2,BackVaRESBTC5,BackVaRESBTC10) %>% 
  data.frame() %>% 
  mutate(method = rep(c("GAS", "MSGARCH", "Boot.", "FIGARCH", "CGARCH",
                        "AVG", "MED", "MIN", "MAX", "$\\rm{RSC_{FZG}}$","$\\rm{RSC_{NZ}}$","$\\rm{RSC_{AL}}$",
                        "$\\rm{MSC_{FZG}}$","$\\rm{MSC_{NZ}}$","$\\rm{MSC_{AL}}$"),3)) %>% 
  mutate(risk_level = c(rep("2.5\\%",15), rep("5\\%",15), rep("10\\%",15))) %>% 
  mutate(how_many_ct = ifelse(CC>p & ESR_2 > p &  ESR_3 > p & VQ > p  & MFE>p & NZ>p,1,0)) %>% 
  mutate(classe = rep(c(rep("Indiv.",5), rep("Comb.",10)),3)) 

VaRESETH = rbind(BackVaRESETH2,BackVaRESETH5,BackVaRESETH10) %>% 
  data.frame() %>% 
  mutate(method = rep(c("GAS", "MSGARCH", "Boot.", "FIGARCH", "CGARCH",
                        "AVG", "MED", "MIN", "MAX", "$\\rm{RSC_{FZG}}$","$\\rm{RSC_{NZ}}$","$\\rm{RSC_{AL}}$",
                        "$\\rm{MSC_{FZG}}$","$\\rm{MSC_{NZ}}$","$\\rm{MSC_{AL}}$"),3)) %>% 
  mutate(risk_level = c(rep("2.5\\%",15), rep("5\\%",15), rep("10\\%",15))) %>% 
  mutate(how_many_ct = ifelse(CC>p  &  ESR_2 > p &  ESR_3 > p &VQ > p  & MFE>p & NZ>p,1,0)) %>% 
  mutate(classe = rep(c(rep("Indiv.",5), rep("Comb.",10)),3)) 

setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/WP_2021")
VaRESBTC %>% 
  select(risk_level, classe, method, Hits, CC, VQ,MFE,NZ, ESR_2, ESR_3,AQL,AFZG,ANZ,AAL) %>% 
  kbl(format = "latex", digits = c(1,1,1,1,3,3,3,3,3,3,4,4,4,4), booktabs = T,
      row.names = FALSE, escape = FALSE, caption = Caption_btc, label = label_name_btc,
      col.names = c("","","","Hits", "CC", "VQ", "ER","CoC", "ESR2", "ESR3", "QL", "FZG","NZ", "AL")) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle",row_group_label_position = "identity") %>% 
  add_header_above(c("", " ", " ", " ", "Calibration tests" = 6, "Average Scoring functions" = 4)) %>% 
  column_spec(3:10, background = ifelse(VaRESBTC$how_many_ct == 1, "gray!25", "white")) %>% 
  column_spec(11, bold = ifelse(MCCQL_BTC == 1, T, F)) %>% 
  column_spec(12, bold = ifelse(MCCFZG_BTC == 1, T, F)) %>% 
  column_spec(13, bold = ifelse(MCCNZ_BTC == 1, T, F)) %>% 
  column_spec(14, bold = ifelse(MCCAL_BTC == 1, T, F)) %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 7) %>% 
  save_kable(keep_tex = T, file = file_tex_name_btc)



VaRESETH %>% 
  select(risk_level, classe, method, Hits, CC, VQ,MFE,NZ, ESR_2, ESR_3,AQL,AFZG,ANZ,AAL) %>% 
  kbl(format = "latex", digits = c(1,1,1,1,3,3,3,3,3,3,4,4,4,4), booktabs = T,
      row.names = FALSE, escape = FALSE, caption = Caption_eth, label = label_name_eth,
      col.names = c("","","","Hits", "CC", "VQ", "ER","CoC",  "ESR2", "ESR3", "QL", "FZG","NZ", "AL")) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle",row_group_label_position = "identity") %>% 
  add_header_above(c("", " ", " ", " ", "Calibration tests" = 6, "Average Scoring functions" = 4)) %>% 
  column_spec(3:10, background = ifelse(VaRESETH$how_many_ct == 1, "gray!25", "white")) %>% 
  column_spec(11, bold = ifelse(MCCQL_ETH == 1, T, F)) %>% 
  column_spec(12, bold = ifelse(MCCFZG_ETH == 1, T, F)) %>% 
  column_spec(13, bold = ifelse(MCCNZ_ETH == 1, T, F)) %>% 
  column_spec(14, bold = ifelse(MCCAL_ETH == 1, T, F)) %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 7) %>% 
  save_kable(keep_tex = T, file = file_tex_name_eth)


#############################################
### Giacomini and Rossi Fluctuation Test  ###
#############################################
library(ggplot2)
library(patchwork)
m <- round(mu_ * OoS)
days <- seq(as.Date("2019-12-23") + m - 1, as.Date(end_date) - 1,"day")

graficos_fluctuations = function(modelo1, modelo2, crypto, risklevel, fluc_alpha = 0.05){
  a = ifelse(risklevel == 2.5, 0.025, ifelse(risklevel == 5, 0.050, 0.100))
  if (risklevel == 2.5){
    if (crypto == "BTC"){
      VaR_ = VaRBTC2; ES_ = ESBTC2; ret_ = retBTC
    } 
    if (crypto == "ETH"){
      VaR_ = VaRETH2; ES_ = ESETH2; ret_ = retBTC
    } 
  }
  if (risklevel == 5){
    if (crypto == "BTC"){
      VaR_ = VaRBTC5; ES_ = ESBTC5; ret_ = retBTC
    } 
    if (crypto == "ETH"){
      VaR_ = VaRETH5; ES_ = ESETH5; ret_ = retBTC
    } 
  }
  if (risklevel == 10){
    if (crypto == "BTC"){
      VaR_ = VaRBTC10; ES_ = ESBTC10; ret_ = retBTC
    } 
    if (crypto == "ETH"){
      VaR_ = VaRETH10; ES_ = ESETH10; ret_ = retBTC
    } 
  }

  GR_QL <- fluct_test(QL(matrix(VaR_[,modelo1], ncol = 1), ret_, alpha = a), 
                      QL(matrix(VaR_[,modelo2], ncol = 1), ret_, alpha = a),
                      mu = mu_, alpha = fluc_alpha, dmv_fullsample = TRUE)
  GR_FZG <- fluct_test(FZG(matrix(VaR_[,modelo1], ncol = 1), matrix(ES_[,modelo1], ncol = 1), ret_, alpha = a), 
                       FZG(matrix(VaR_[,modelo2], ncol = 1), matrix(ES_[,modelo2], ncol = 1), ret_, alpha = a),
                       mu = mu_, alpha = fluc_alpha, dmv_fullsample = TRUE)
  GR_NZ <- fluct_test(NZ(matrix(VaR_[,modelo1], ncol = 1), matrix(ES_[,modelo1], ncol = 1), ret_, alpha = a), 
                      NZ(matrix(VaR_[,modelo2], ncol = 1), matrix(ES_[,modelo2], ncol = 1), ret_, alpha = a),
                      mu = mu_,  alpha = fluc_alpha, dmv_fullsample = TRUE)
  GR_AL <- fluct_test(AL(matrix(VaR_[,modelo1], ncol = 1), matrix(ES_[,modelo1], ncol = 1), ret_, alpha = a), 
                      AL(matrix(VaR_[,modelo2], ncol = 1), matrix(ES_[,modelo2], ncol = 1), ret_, alpha = a),
                      mu = mu_,  alpha = fluc_alpha, dmv_fullsample = TRUE)
  
  
  if(modelo2 == "FZG_RSC"){
  plot1 <- ggplot(data = GR_QL$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[FZG]~". QL scoring function, risk level"~.(risklevel)~"%."))
  plot2 <-ggplot(data = GR_FZG$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[FZG]~". FZG scoring function, risk level"~.(risklevel)~"%."))
  plot3 <-ggplot(data = GR_NZ$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[FZG]~". NZ scoring function, risk level"~.(risklevel)~"%."))
  plot4 <-ggplot(data = GR_AL$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[FZG]~". AL scoring function, risk level"~.(risklevel)~"%."))
  } else{
    if(modelo2 == "NZ_RSC"){
      plot1 <- ggplot(data = GR_QL$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[NZ]~". QL scoring function, risk level"~.(risklevel)~"%."))
      plot2 <-ggplot(data = GR_FZG$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[NZ]~". FZG scoring function, risk level"~.(risklevel)~"%."))
      plot3 <-ggplot(data = GR_NZ$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[NZ]~". NZ scoring function, risk level"~.(risklevel)~"%."))
      plot4 <-ggplot(data = GR_AL$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[NZ]~". AL scoring function, risk level"~.(risklevel)~"%."))
      
    } else {
      if(modelo2 == "AL_RSC"){
        plot1 <- ggplot(data = GR_QL$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[AL]~". QL scoring function, risk level"~.(risklevel)~"%."))
        plot2 <-ggplot(data = GR_FZG$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[AL]~". FZG scoring function, risk level"~.(risklevel)~"%."))
        plot3 <-ggplot(data = GR_NZ$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[AL]~". NZ scoring function, risk level"~.(risklevel)~"%."))
        plot4 <-ggplot(data = GR_AL$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RSC[AL]~". AL scoring function, risk level"~.(risklevel)~"%."))
      } else{
        if(modelo2 == "FZG_MSC"){
          plot1 <- ggplot(data = GR_QL$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[FZG]~". QL scoring function, risk level"~.(risklevel)~"%."))
          plot2 <-ggplot(data = GR_FZG$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~RMSC[FZG]~". FZG scoring function, risk level"~.(risklevel)~"%."))
          plot3 <-ggplot(data = GR_NZ$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[FZG]~". NZ scoring function, risk level"~.(risklevel)~"%."))
          plot4 <-ggplot(data = GR_AL$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[FZG]~". AL scoring function, risk level"~.(risklevel)~"%."))
        } else{
          if(modelo2 == "NZ_MSC"){
            plot1 <- ggplot(data = GR_QL$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[NZ]~". QL scoring function, risk level"~.(risklevel)~"%."))
            plot2 <-ggplot(data = GR_FZG$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[NZ]~". FZG scoring function, risk level"~.(risklevel)~"%."))
            plot3 <-ggplot(data = GR_NZ$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[NZ]~". NZ scoring function, risk level"~.(risklevel)~"%."))
            plot4 <-ggplot(data = GR_AL$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[NZ]~". AL scoring function, risk level"~.(risklevel)~"%."))
          } else{
            if(modelo2 == "AL_MSC"){
              plot1 <- ggplot(data = GR_QL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[AL]~". QL scoring function, risk level"~.(risklevel)~"%."))
              plot2 <-ggplot(data = GR_FZG$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[AL]~". FZG scoring function, risk level"~.(risklevel)~"%."))
              plot3 <-ggplot(data = GR_NZ$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[AL]~". NZ scoring function, risk level"~.(risklevel)~"%."))
              plot4 <-ggplot(data = GR_AL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~MSC[AL]~". AL scoring function, risk level"~.(risklevel)~"%."))
            } else{
              plot1 <- ggplot(data = GR_QL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~.(modelo2)~". QL scoring function, risk level"~.(risklevel)~"%."))
              plot2 <-ggplot(data = GR_FZG$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~.(modelo2)~". FZG scoring function, risk level"~.(risklevel)~"%."))
              plot3 <-ggplot(data = GR_NZ$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~.(modelo2)~". NZ scoring function, risk level"~.(risklevel)~"%."))
              plot4 <-ggplot(data = GR_AL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed")+ 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(crypto)~": "~.(modelo1)~"vs"~.(modelo2)~". AL scoring function, risk level"~.(risklevel)~"%."))
            }
          }
        }
      }
    }
  }
  plot1 + plot2 + plot3 + plot4
  ggsave(paste0(crypto,"_",modelo1, "_", modelo2, "_", round(risklevel,0), ".pdf"), width = 35, height = 21, units = "cm")
}

# BTC
methods = c("FZG_RSC", "NZ_RSC", "AL_RSC", "FZG_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
     graficos_fluctuations(methods[i], methods[j], "BTC", 2.5)
  }
}

methods = c("FZG_RSC", "FZG_MSC", "NZ_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
     graficos_fluctuations(methods[i], methods[j], "BTC", 5)
  }
}

methods = c("GAS", "AVG", "FZG_RSC", "NZ_RSC", "AL_RSC", "FZG_MSC", "NZ_MSC", "AL_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "BTC", 10)
  }
}

# ETH
methods = c("Boot.", "FIGARCH", "AVG", "MAX","FZG_RSC", "NZ_RSC", "AL_RSC", "FZG_MSC", "NZ_MSC", "AL_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "ETH", 2.5)
  }
}

methods = c("Boot.", "FIGARCH", "CGARCH", "AVG", "MED", "MIN", "FZG_RSC", "NZ_RSC", "FZG_MSC", "NZ_MSC", "AL_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "ETH", 5)
  }
}

methods = c("GAS", "MSGARCH", "Boot.", "FIGARCH", "CGARCH", "AVG", "MED", "MIN", "FZG_RSC", "NZ_RSC", "AL_RSC", "FZG_MSC", "NZ_MSC", "AL_MSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "ETH", 10)
  }
}
