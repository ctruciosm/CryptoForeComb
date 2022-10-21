################################################################################## 
## Paper: A Comparison of Methods for Forecasting Value-at-Risk and Expected    ##
##        Shortfall of Cryptocurrencies.                                        ##
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
    select(ends_with(risklevel)) %>%
    mutate(FZG_RSC = VaR_RSC_FZG[,rl], NZ_RSC = VaR_RSC_NZ[,rl], AL_RSC = VaR_RSC_AL[,rl],
           FZG_MSC = VaR_MSC_FZG[,rl], NZ_MSC = VaR_MSC_NZ[,rl], AL_MSC = VaR_MSC_AL[,rl])
  
  ES_full = ES %>% data.frame() %>% 
    select(ends_with(risklevel)) %>%
    mutate(FZG_RSC = ES_RSC_FZG[,rl], NZ_RSC = ES_RSC_NZ[,rl], AL_RSC = ES_RSC_AL[,rl],
           FZG_MSC = ES_MSC_FZG[,rl], NZ_MSC = ES_MSC_NZ[,rl], AL_MSC = ES_MSC_AL[,rl])
  return(list(ret_OoS,VaR_full, ES_full))
}

n_ind = 10 # number os individual models

library(stringr)
library(modelconf)
library(dplyr)
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
source("Optimizations.R")
source("GiacominiRossiTest.R")


p = 0.05
pMCS = 0.05
mu_ = 0.2
end_date = "2022-07-23"
MCS_type = "t.max"
block_length =  21
out_of_sample = 800

a2 = 0.025; a5 = 0.050; 
Caption_btc = "One-step-ahead VaR and ES backtesting for Bitcoin at 2.5\\% and 5\\% risk levels. Shaded rows indicate procedures with p-values larger than 0.05 in all calibration tests."
file_tex_name_btc = "VaRES_btc.tex" 
label_name_btc = "Table_VaRES_btc"

Caption_eth = "One-step-ahead VaR and ES backtesting for Ethereum at 2.5\\% and 5\\% risk levels. Shaded rows indicate procedures with p-values larger than 0.05 in all calibration tests."
file_tex_name_eth = "VaRES_eth.tex" 
label_name_eth = "Table_VaRES_eth"

setwd("./BTC")
if (str_sub(getwd(), -3, -1) == "BTC") {
  crypto = read.csv("./Data/BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
  OoS = out_of_sample
  InS = dim(crypto)[1] - OoS
  crypto = crypto[(InS + 1):(InS + OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(as.character(2), 1)
  retBTC = AUX[[1]]
  VaRBTC2 = AUX[[2]]
  ESBTC2  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(5), 2)
  VaRBTC5 = AUX[[2]]
  ESBTC5  = AUX[[3]]
}
setwd("./ETH")
if (str_sub(getwd(), -3, -1) == "ETH") {
  crypto = read.csv("./Data/ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
  OoS = out_of_sample
  InS = dim(crypto)[1] - OoS
  crypto = crypto[(InS + 1):(InS + OoS),]
  CovidDay = which(crypto$date == "2020-03-12")
  # Setting 
  AUX = VaR_ES_full(as.character(2), 1)
  retETH = AUX[[1]]
  VaRETH2 = AUX[[2]]
  ESETH2  = AUX[[3]]
  
  AUX = VaR_ES_full(as.character(5), 2)
  VaRETH5 = AUX[[2]]
  ESETH5  = AUX[[3]]
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

### Median
VaRBTC2$MED = apply(VaRBTC2[,1:n_ind],1,median)
VaRETH2$MED = apply(VaRETH2[,1:n_ind],1,median)
ESBTC2$MED = apply(ESBTC2[,1:n_ind],1,median)
ESETH2$MED = apply(ESETH2[,1:n_ind],1,median)

VaRBTC5$MED = apply(VaRBTC5[,1:n_ind],1,median)
VaRETH5$MED = apply(VaRETH5[,1:n_ind],1,median)
ESBTC5$MED = apply(ESBTC5[,1:n_ind],1,median)
ESETH5$MED = apply(ESETH5[,1:n_ind],1,median)

### Min
VaRBTC2$MIN = apply(VaRBTC2[,1:n_ind],1,min)
VaRETH2$MIN = apply(VaRETH2[,1:n_ind],1,min)
ESBTC2$MIN = apply(ESBTC2[,1:n_ind],1,min)
ESETH2$MIN = apply(ESETH2[,1:n_ind],1,min)

VaRBTC5$MIN = apply(VaRBTC5[,1:n_ind],1,min)
VaRETH5$MIN = apply(VaRETH5[,1:n_ind],1,min)
ESBTC5$MIN = apply(ESBTC5[,1:n_ind],1,min)
ESETH5$MIN = apply(ESETH5[,1:n_ind],1,min)

### Max
VaRBTC2$MAX = apply(VaRBTC2[,1:n_ind],1,max)
VaRETH2$MAX = apply(VaRETH2[,1:n_ind],1,max)
ESBTC2$MAX = apply(ESBTC2[,1:n_ind],1,max)
ESETH2$MAX = apply(ESETH2[,1:n_ind],1,max)

VaRBTC5$MAX = apply(VaRBTC5[,1:n_ind],1,max)
VaRETH5$MAX = apply(VaRETH5[,1:n_ind],1,max)
ESBTC5$MAX = apply(ESBTC5[,1:n_ind],1,max)
ESETH5$MAX = apply(ESETH5[,1:n_ind],1,max)

###########################################
# Reordering
ordering2 <- c(paste0("GARCH","2"), paste0("GJR","2"), paste0("GAS","2"),paste0("MSGARCH","2"),paste0("Boot","2"),
              paste0("FIGARCH","2"),paste0("NGARCH","2"),paste0("CAViaR","2"),
              paste0("CAViaREVT","2"), paste0("CAViaRALD","2"),
              "AVG", "MED", "MAX", "MIN",
              "FZG_MSC","NZ_MSC", "AL_MSC", "FZG_RSC","NZ_RSC","AL_RSC")

ordering5 <- c(paste0("GARCH","5"),paste0("GJR","5"),paste0("GAS","5"),paste0("MSGARCH","5"),paste0("Boot","5"),
               paste0("FIGARCH","5"),paste0("NGARCH","5"),paste0("CAViaR","5"),
               paste0("CAViaREVT","5"), paste0("CAViaRALD","5"),
               "AVG", "MED", "MAX", "MIN",
               "FZG_MSC","NZ_MSC", "AL_MSC", "FZG_RSC","NZ_RSC","AL_RSC")


names = c("GARCH", "GJR", "GAS", "MSGARCH", "Boot.", "FIGARCH", "NGARCH", "CAViaR", "CAViaREVT",
          "CAViaRALD", "AVG", "MED", "MAX", "MIN", 
          "FZG_MSC","NZ_MSC", "AL_MSC", "FZG_RSC","NZ_RSC","AL_RSC")

VaRBTC2 <- VaRBTC2 %>% select(ordering2)
colnames(VaRBTC2) = names

VaRETH2 <- VaRETH2 %>% select(ordering2)
colnames(VaRETH2) = names

VaRBTC5 <- VaRBTC5 %>% select(ordering5)
colnames(VaRBTC5) = names

VaRETH5 <- VaRETH5 %>% select(ordering5)
colnames(VaRETH5) = names

ESBTC2 <- ESBTC2 %>% select(ordering2)
colnames(ESBTC2) = names

ESETH2 <- ESETH2 %>% select(ordering2)
colnames(ESETH2) = names

ESBTC5 <- ESBTC5 %>% select(ordering5)
colnames(ESBTC5) = names

ESETH5 <- ESETH5 %>% select(ordering5)
colnames(ESETH5) = names


###########################################
#########   VaR Backtesting      ##########
###########################################
K = dim(VaRBTC2)[2]  

BackVaRESBTC2 = BackVaRESETH2 = BackVaRESBTC5 = BackVaRESETH5 = matrix(0,ncol = 14,nrow = K) 
colnames(BackVaRESBTC2) = colnames(BackVaRESETH2) = colnames(BackVaRESBTC5) = colnames(BackVaRESETH5) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESR_1", "ESR_2" ,"ESR_3", "AQL", "AFZG", "ANZ", "AAL")

for (i in 1:K) { # each i is a method (individual or combination)
  print(i)
  set.seed(1234) # Only for reproducible porpuses
  BackTBTC2 = BacktestVaR(retBTC, VaRBTC2[,i], alpha = a2, Lags = 4)
  BackTBTC5 = BacktestVaR(retBTC, VaRBTC5[,i], alpha = a5, Lags = 4)
  BackTETH2 = BacktestVaR(retETH, VaRETH2[,i], alpha = a2, Lags = 4)
  BackTETH5 = BacktestVaR(retETH, VaRETH5[,i], alpha = a5, Lags = 4)

  EBackTBTC2 = ESTest(alpha = a2, retBTC, ESBTC2[,i], VaRBTC2[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTBTC5 = ESTest(alpha = a5, retBTC, ESBTC5[,i], VaRBTC5[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH2 = ESTest(alpha = a2, retETH, ESETH2[,i], VaRETH2[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)
  EBackTETH5 = ESTest(alpha = a5, retETH, ESETH5[,i], VaRETH5[,i], conf.level = 0.95,  boot = TRUE, n.boot = 5000)

  BackVaRESBTC2[i,] = c(mean(retBTC < VaRBTC2[,i])*100, 
                       BackTBTC2$LRuc[2], BackTBTC2$LRcc[2],BackTBTC2$DQ$pvalue, VaR_VQR(retBTC, VaRBTC2[,i], a2),
                       EBackTBTC2$boot.p.value,
                       cc_backtest(retBTC, VaRBTC2[,i], ESBTC2[,i], alpha  = a2)$pvalue_twosided_simple, 
                       esr_backtest(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic,
                       esr_backtest(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic,
                       esr_backtest(retBTC, VaRBTC2[,i], ESBTC2[,i],alpha  = a2, B = 0, version = 3)$pvalue_onesided_asymptotic,
                       mean(QL(matrix(VaRBTC2[,i], ncol = 1) ,retBTC, alpha = a2)),
                       mean(FZG(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)),
                       mean(NZ(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)),
                       mean(AL(matrix(VaRBTC2[,i], ncol = 1), matrix(ESBTC2[,i], ncol = 1), retBTC, alpha = a2)))
  
  BackVaRESBTC5[i,] = c(mean(retBTC < VaRBTC5[,i])*100, 
                        BackTBTC5$LRuc[2], BackTBTC5$LRcc[2],BackTBTC5$DQ$pvalue, VaR_VQR(retBTC, VaRBTC5[,i], a5),
                        EBackTBTC5$boot.p.value,
                        cc_backtest(retBTC, VaRBTC5[,i], ESBTC5[,i],  alpha  = a5)$pvalue_twosided_simple, 
                        esr_backtest(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic,
                        esr_backtest(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic,
                        esr_backtest(retBTC, VaRBTC5[,i], ESBTC5[,i],alpha  = a5, B = 0, version = 3)$pvalue_onesided_asymptotic,
                        mean(QL(matrix(VaRBTC5[,i], ncol = 1) ,retBTC, alpha = a5)),
                        mean(FZG(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)),
                        mean(NZ(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)),
                        mean(AL(matrix(VaRBTC5[,i], ncol = 1), matrix(ESBTC5[,i], ncol = 1), retBTC, alpha = a5)))
  
  BackVaRESETH2[i,] = c(mean(retETH < VaRETH2[,i])*100,
                       BackTETH2$LRuc[2], BackTETH2$LRcc[2],BackTETH2$DQ$pvalue, VaR_VQR(retETH, VaRETH2[,i], a2),
                       EBackTETH2$boot.p.value,
                       cc_backtest(retETH, VaRETH2[,i], ESETH2[,i], alpha  = a2)$pvalue_twosided_simple,
                       esr_backtest(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic,
                       esr_backtest(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic,
                       esr_backtest(retETH, VaRETH2[,i], ESETH2[,i],alpha  = a2, B = 0, version = 3)$pvalue_onesided_asymptotic,
                       mean(QL(matrix(VaRETH2[,i], ncol = 1),retETH, alpha = a2)),
                       mean(FZG(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)),
                       mean(NZ(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)),
                       mean(AL(matrix(VaRETH2[,i], ncol = 1), matrix(ESETH2[,i], ncol = 1), retETH, alpha = a2)))

  BackVaRESETH5[i,] = c(mean(retETH < VaRETH5[,i])*100,
                        BackTETH5$LRuc[2], BackTETH5$LRcc[2],BackTETH5$DQ$pvalue, VaR_VQR(retETH, VaRETH5[,i], a5),
                        EBackTETH5$boot.p.value,
                        cc_backtest(retETH, VaRETH5[,i], ESETH5[,i],  alpha  = a5)$pvalue_twosided_simple,
                        esr_backtest(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic,
                        esr_backtest(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic,
                        esr_backtest(retETH, VaRETH5[,i], ESETH5[,i],alpha  = a5, B = 0, version = 3)$pvalue_onesided_asymptotic,
                        mean(QL(matrix(VaRETH5[,i], ncol = 1),retETH, alpha = a5)),
                        mean(FZG(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)),
                        mean(NZ(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)),
                        mean(AL(matrix(VaRETH5[,i], ncol = 1), matrix(ESETH5[,i], ncol = 1), retETH, alpha = a5)))

}

###########################################
#######    Model Confidence Set      ######
###########################################


MCSBTC_MQL2 = rep(0,ncol(VaRBTC2))
MQL2 = QL(as.matrix(VaRBTC2),retBTC, alpha = a2)
colnames(MQL2) = colnames(VaRBTC2)
auxBTC_MQL2 = estMCS.quick(MQL2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MQL2[auxBTC_MQL2] = 1

MCSBTC_MQL5 = rep(0,ncol(VaRBTC5))
MQL5 = QL(as.matrix(VaRBTC5),retBTC, alpha = a5)
colnames(MQL5) = colnames(VaRBTC5)
auxBTC_MQL5 = estMCS.quick(MQL5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MQL5[auxBTC_MQL5] = 1


MCSBTC_MFZG2 = rep(0,ncol(VaRBTC2))
MFZG2 = FZG(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MFZG2) = colnames(VaRBTC2)
auxBTC_MFZG2 = estMCS.quick(MFZG2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MFZG2[auxBTC_MFZG2] = 1

MCSBTC_MFZG5 = rep(0,ncol(VaRBTC5))
MFZG5 = FZG(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MFZG5) = colnames(VaRBTC5)
auxBTC_MFZG5 = estMCS.quick(MFZG5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MFZG5[auxBTC_MFZG5] = 1


MCSBTC_MNZ2 = rep(0,ncol(VaRBTC2))
MNZ2 = NZ(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MNZ2) = colnames(VaRBTC2)
auxBTC_MNZ2 = estMCS.quick(MNZ2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MNZ2[auxBTC_MNZ2] = 1

MCSBTC_MNZ5 = rep(0,ncol(VaRBTC5))
MNZ5 = NZ(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MNZ5) = colnames(VaRBTC5)
auxBTC_MNZ5 = estMCS.quick(MNZ5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MNZ5[auxBTC_MNZ5] = 1


MCSBTC_MAL2 = rep(0,ncol(VaRBTC2))
MAL2 = AL(as.matrix(VaRBTC2), as.matrix(ESBTC2), retBTC, alpha = a2)
colnames(MAL2) = colnames(VaRBTC2)
auxBTC_MAL2 = estMCS.quick(MAL2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MAL2[auxBTC_MAL2] = 1

MCSBTC_MAL5 = rep(0,ncol(VaRBTC5))
MAL5 = AL(as.matrix(VaRBTC5), as.matrix(ESBTC5), retBTC, alpha = a5)
colnames(MAL5) = colnames(VaRBTC5)
auxBTC_MAL5 = estMCS.quick(MAL5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSBTC_MAL5[auxBTC_MAL5] = 1


MCSETH_MQL2 = rep(0,ncol(VaRETH2))
MQL2 = QL(as.matrix(VaRETH2),retETH, alpha = a2)
colnames(MQL2) = colnames(VaRETH2)
auxETH_MQL2 = estMCS.quick(MQL2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MQL2[auxETH_MQL2] = 1

MCSETH_MQL5 = rep(0,ncol(VaRETH5))
MQL5 = QL(as.matrix(VaRETH5),retETH, alpha = a5)
colnames(MQL5) = colnames(VaRETH5)
auxETH_MQL5 = estMCS.quick(MQL5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MQL5[auxETH_MQL5] = 1


MCSETH_MFZG2 = rep(0,ncol(VaRETH2))
MFZG2 = FZG(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MFZG2) = colnames(VaRETH2)
auxETH_MFZG2 = estMCS.quick(MFZG2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MFZG2[auxETH_MFZG2] = 1

MCSETH_MFZG5 = rep(0,ncol(VaRETH5))
MFZG5 = FZG(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MFZG5) = colnames(VaRETH5)
auxETH_MFZG5 = estMCS.quick(MFZG5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MFZG5[auxETH_MFZG5] = 1


MCSETH_MNZ2 = rep(0,ncol(VaRETH2))
MNZ2 = NZ(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MNZ2) = colnames(VaRETH2)
auxETH_MNZ2 = estMCS.quick(MNZ2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MNZ2[auxETH_MNZ2] = 1

MCSETH_MNZ5 = rep(0,ncol(VaRETH5))
MNZ5 = NZ(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MNZ5) = colnames(VaRETH5)
auxETH_MNZ5 = estMCS.quick(MNZ5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MNZ5[auxETH_MNZ5] = 1

MCSETH_MAL2 = rep(0,ncol(VaRETH2))
MAL2 = AL(as.matrix(VaRETH2), as.matrix(ESETH2), retETH, alpha = a2)
colnames(MAL2) = colnames(VaRETH2)
auxETH_MAL2 = estMCS.quick(MAL2, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MAL2[auxETH_MAL2] = 1

MCSETH_MAL5 = rep(0,ncol(VaRETH5))
MAL5 = AL(as.matrix(VaRETH5), as.matrix(ESETH5), retETH, alpha = a5)
colnames(MAL5) = colnames(VaRETH5)
auxETH_MAL5 = estMCS.quick(MAL5, test = MCS_type, B = 5000, l = block_length, alpha = pMCS)
MCSETH_MAL5[auxETH_MAL5] = 1


MCCQL_BTC = c(MCSBTC_MQL2,MCSBTC_MQL5)
MCCFZG_BTC = c(MCSBTC_MFZG2,MCSBTC_MFZG5)
MCCNZ_BTC = c(MCSBTC_MNZ2,MCSBTC_MNZ5)
MCCAL_BTC = c(MCSBTC_MAL2,MCSBTC_MAL5)

MCCQL_ETH = c(MCSETH_MQL2,MCSETH_MQL5)
MCCFZG_ETH = c(MCSETH_MFZG2,MCSETH_MFZG5)
MCCNZ_ETH = c(MCSETH_MNZ2,MCSETH_MNZ5)
MCCAL_ETH = c(MCSETH_MAL2,MCSETH_MAL5)


#############################################
###       Latex Table format              ###
#############################################


VaRESBTC = rbind(BackVaRESBTC2,BackVaRESBTC5) %>% 
  data.frame() %>% 
  mutate(method = rep(c("GARCH", "GJR", "GAS", "MSGARCH", "Boot.", "FIGARCH", "NGARCH", "CAViaR", "CAViaREVT",
                        "CAViaRALD", "AVG", "MED", "MAX", "MIN", 
                        "$\\rm{MSC_{FZG}}$","$\\rm{MSC_{NZ}}$","$\\rm{MSC_{AL}}$",
			"$\\rm{RSC_{FZG}}$","$\\rm{RSC_{NZ}}$","$\\rm{RSC_{AL}}$"),2)) %>% 
  mutate(risk_level = c(rep("2.5\\%",20), rep("5\\%",20))) %>% 
  mutate(how_many_ct = ifelse(CC > p & DQ > p & VQ > p & ESR_3 > p & MFE > p & NZ > p,1,0)) %>% 
  mutate(classe = rep(c(rep("Indiv.",10), rep("Comb.",10)),2)) 

VaRESETH = rbind(BackVaRESETH2,BackVaRESETH5) %>% 
  data.frame() %>% 
  mutate(method = rep(c("GARCH", "GJR", "GAS", "MSGARCH", "Boot.", "FIGARCH", "NGARCH", "CAViaR", "CAViaREVT",
                        "CAViaRALD", "AVG", "MED", "MAX", "MIN", 
                        "$\\rm{MSC_{FZG}}$","$\\rm{MSC_{NZ}}$","$\\rm{MSC_{AL}}$",
                        "$\\rm{RSC_{FZG}}$","$\\rm{RSC_{NZ}}$","$\\rm{RSC_{AL}}$"),2)) %>% 
  mutate(risk_level = c(rep("2.5\\%",20), rep("5\\%",20))) %>% 
  mutate(how_many_ct = ifelse(CC > p & DQ > p & VQ > p & ESR_3 > p & MFE > p & NZ > p,1,0)) %>% 
  mutate(classe = rep(c(rep("Indiv.",10), rep("Comb.",10)),2)) 


VaRESBTC %>% 
  select(risk_level, classe, method, Hits, CC, DQ,VQ,MFE,NZ, ESR_3, AQL,AFZG,ANZ,AAL) %>% 
  kbl(format = "latex", digits = c(1,1,1,1,3,3,3,3,3,3,3,3,3,3), booktabs = T,
      row.names = FALSE, escape = FALSE, caption = Caption_btc, label = label_name_btc,
      col.names = c("","","","Hits", "CC", "DQ", "VQ", "ER","CoC", "ESR", "QL", "FZG","NZ", "AL")) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle",row_group_label_position = "identity") %>% 
  add_header_above(c("", " ", " ", " ", "Calibration test p-values" = 6, "Average scoring functions" = 4)) %>% 
  column_spec(3:10, background = ifelse(VaRESBTC$how_many_ct == 1, "gray!25", "white")) %>% 
  column_spec(11, bold = ifelse(MCCQL_BTC == 1, T, F)) %>% 
  column_spec(12, bold = ifelse(MCCFZG_BTC == 1, T, F)) %>% 
  column_spec(13, bold = ifelse(MCCNZ_BTC == 1, T, F)) %>% 
  column_spec(14, bold = ifelse(MCCAL_BTC == 1, T, F)) %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 7) %>% 
  save_kable(keep_tex = T, file = file_tex_name_btc)



VaRESETH %>% 
  select(risk_level, classe, method, Hits, CC, DQ,VQ,MFE,NZ, ESR_3, AQL,AFZG,ANZ,AAL) %>% 
  kbl(format = "latex", digits = c(1,1,1,1,3,3,3,3,3,3,3,3,3,3), booktabs = T,
      row.names = FALSE, escape = FALSE, caption = Caption_eth, label = label_name_eth,
      col.names = c("","","","Hits", "CC", "DQ", "VQ", "ER","CoC", "ESR", "QL", "FZG","NZ", "AL")) %>% 
  collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle",row_group_label_position = "identity") %>% 
  add_header_above(c("", " ", " ", " ", "Calibration test p-values" = 6, "Average scoring functions" = 4)) %>% 
  column_spec(3:10, background = ifelse(VaRESETH$how_many_ct == 1, "gray!25", "white")) %>% 
  column_spec(11, bold = ifelse(MCCQL_ETH == 1, T, F)) %>% 
  column_spec(12, bold = ifelse(MCCFZG_ETH == 1, T, F)) %>% 
  column_spec(13, bold = ifelse(MCCNZ_ETH == 1, T, F)) %>% 
  column_spec(14, bold = ifelse(MCCAL_ETH == 1, T, F)) %>% 
  kable_styling(latex_options = c("scale_down"), font_size = 7) %>% 
  save_kable(keep_tex = T, file = file_tex_name_eth)



which(BackVaRESBTC2[,"AQL"] == min(BackVaRESBTC2[,"AQL"]))
which(BackVaRESBTC2[,"AFZG"] == min(BackVaRESBTC2[,"AFZG"]))
which(BackVaRESBTC2[,"ANZ"] == min(BackVaRESBTC2[,"ANZ"]))
which(BackVaRESBTC2[,"AAL"] == min(BackVaRESBTC2[,"AAL"]))

which(BackVaRESBTC5[,"AQL"] == min(BackVaRESBTC5[,"AQL"]))
which(BackVaRESBTC5[,"AFZG"] == min(BackVaRESBTC5[,"AFZG"]))
which(BackVaRESBTC5[,"ANZ"] == min(BackVaRESBTC5[,"ANZ"]))
which(BackVaRESBTC5[,"AAL"] == min(BackVaRESBTC5[,"AAL"]))


which(BackVaRESETH2[,"AQL"] == min(BackVaRESETH2[,"AQL"]))
which(BackVaRESETH2[,"AFZG"] == min(BackVaRESETH2[,"AFZG"]))
which(BackVaRESETH2[,"ANZ"] == min(BackVaRESETH2[,"ANZ"]))
which(BackVaRESETH2[,"AAL"] == min(BackVaRESETH2[,"AAL"]))

which(BackVaRESETH5[,"AQL"] == min(BackVaRESETH5[,"AQL"]))
which(BackVaRESETH5[,"AFZG"] == min(BackVaRESETH5[,"AFZG"]))
which(BackVaRESETH5[,"ANZ"] == min(BackVaRESETH5[,"ANZ"]))
which(BackVaRESETH5[,"AAL"] == min(BackVaRESETH5[,"AAL"]))

#############################################
### Giacomini and Rossi Fluctuation Test  ###
#############################################
library(ggplot2)
library(patchwork)
mu_ <- 0.2
m <- round(mu_ * OoS)
days <- seq(as.Date("2020-05-14") + m - 1, as.Date(end_date) - 1,"day")

graficos_fluctuations = function(modelo1, modelo2, crypto, risklevel, fluc_alpha = 0.05){
  a = ifelse(risklevel == 2.5, 0.025, ifelse(risklevel == 5, 0.050, 0.100))
  if (risklevel == 2.5) {
    if (crypto == "BTC") {
      VaR_ = VaRBTC2; ES_ = ESBTC2; ret_ = retBTC
    } 
    if (crypto == "ETH") {
      VaR_ = VaRETH2; ES_ = ESETH2; ret_ = retBTC
    } 
  }
  if (risklevel == 5) {
    if (crypto == "BTC") {
      VaR_ = VaRBTC5; ES_ = ESBTC5; ret_ = retBTC
    } 
    if (crypto == "ETH") {
      VaR_ = VaRETH5; ES_ = ESETH5; ret_ = retBTC
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
  
  
  if (modelo2 == "FZG_RSC") {
  plot1 <- ggplot(data = GR_QL$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(modelo1)~"vs"~RSC[FZG]~"."),
            subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
    theme(text = element_text(size = 16)) 
  plot2 <- ggplot(data = GR_FZG$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(modelo1)~"vs"~RSC[FZG]~"."),
            subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
    theme(text = element_text(size = 16)) 
  plot3 <- ggplot(data = GR_NZ$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(modelo1)~"vs"~RSC[FZG]~"."),
            subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
    theme(text = element_text(size = 16)) 
  plot4 <- ggplot(data = GR_AL$fluc) + 
    geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
    geom_line(aes(x = days, y), color = "green4") + 
    geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
    ylab("Relative performance") + xlab(" ") + 
    ggtitle(bquote(.(modelo1)~"vs"~RSC[FZG]~"."),
            subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
    theme(text = element_text(size = 16)) 
  } else{
    if (modelo2 == "NZ_RSC") {
      plot1 <- ggplot(data = GR_QL$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(modelo1)~"vs"~RSC[NZ]~"."),
                subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
        theme(text = element_text(size = 16)) 
      plot2 <- ggplot(data = GR_FZG$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(modelo1)~"vs"~RSC[NZ]~"."),
                subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
        theme(text = element_text(size = 16)) 
      plot3 <- ggplot(data = GR_NZ$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(modelo1)~"vs"~RSC[NZ]~"."),
                subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
        theme(text = element_text(size = 16)) 
      plot4 <- ggplot(data = GR_AL$fluc) + 
        geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
        geom_line(aes(x = days, y), color = "green4") + 
        geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
        ylab("Relative performance") + xlab(" ") + 
        ggtitle(bquote(.(modelo1)~"vs"~RSC[NZ]~"."),
                subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
        theme(text = element_text(size = 16)) 
    } else {
      if (modelo2 == "AL_RSC") {
        plot1 <- ggplot(data = GR_QL$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(modelo1)~"vs"~RSC[AL]~"."),
                  subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
          theme(text = element_text(size = 16)) 
        plot2 <- ggplot(data = GR_FZG$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(modelo1)~"vs"~RSC[AL]~"."),
                  subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
          theme(text = element_text(size = 16)) 
        plot3 <- ggplot(data = GR_NZ$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(modelo1)~"vs"~RSC[AL]~"."),
                  subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
          theme(text = element_text(size = 16)) 
        plot4 <- ggplot(data = GR_AL$fluc) + 
          geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
          geom_line(aes(x = days, y), color = "green4") + 
          geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
          ylab("Relative performance") + xlab(" ") + 
          ggtitle(bquote(.(modelo1)~"vs"~RSC[AL]~"."),
                  subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) +
          theme(text = element_text(size = 16)) 
      } else{
        if (modelo2 == "FZG_MSC") {
          plot1 <- ggplot(data = GR_QL$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(modelo1)~"vs"~MSC[FZG]~"."),
                    subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
            theme(text = element_text(size = 16)) 
          plot2 <- ggplot(data = GR_FZG$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(modelo1)~"vs"~MSC[FZG]~"."),
                    subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
            theme(text = element_text(size = 16)) 
          plot3 <- ggplot(data = GR_NZ$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(modelo1)~"vs"~MSC[FZG]~"."),
                    subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
            theme(text = element_text(size = 16)) 
          plot4 <- ggplot(data = GR_AL$fluc) + 
            geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
            geom_line(aes(x = days, y), color = "green4") + 
            geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
            ylab("Relative performance") + xlab(" ") + 
            ggtitle(bquote(.(modelo1)~"vs"~MSC[FZG]~"."),
                    subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
            theme(text = element_text(size = 16)) 
        } else{
          if (modelo2 == "NZ_MSC") {
            plot1 <- ggplot(data = GR_QL$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(modelo1)~"vs"~MSC[NZ]~"."),
                      subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
              theme(text = element_text(size = 16)) 
            plot2 <- ggplot(data = GR_FZG$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(modelo1)~"vs"~MSC[NZ]~"."),
                      subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
              theme(text = element_text(size = 16)) 
            plot3 <- ggplot(data = GR_NZ$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(modelo1)~"vs"~MSC[NZ]~"."),
                      subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
              theme(text = element_text(size = 16)) 
            plot4 <- ggplot(data = GR_AL$fluc) + 
              geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
              geom_line(aes(x = days, y), color = "green4") + 
              geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
              ylab("Relative performance") + xlab(" ") + 
              ggtitle(bquote(.(modelo1)~"vs"~MSC[NZ]~"."),
                      subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
              theme(text = element_text(size = 16)) 
          } else{
            if (modelo2 == "AL_MSC") {
              plot1 <- ggplot(data = GR_QL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~MSC[AL]~"."),
                        subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16)) 
              plot2 <- ggplot(data = GR_FZG$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~MSC[AL]~"."),
                        subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16)) 
              plot3 <- ggplot(data = GR_NZ$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~MSC[AL]~"."),
                        subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16)) 
              plot4 <- ggplot(data = GR_AL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~MSC[AL]~"."),
                        subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16)) 
            } else{
              plot1 <- ggplot(data = GR_QL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_QL$cv_sup, GR_QL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~.(modelo2)~"."),
                        subtitle = bquote("QL scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16))
              plot2 <- ggplot(data = GR_FZG$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_FZG$cv_sup, GR_FZG$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~.(modelo2)~"."),
                        subtitle = bquote("FZG scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16))
              plot3 <- ggplot(data = GR_NZ$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_NZ$cv_sup, GR_NZ$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~.(modelo2)~"."),
                        subtitle = bquote("NZ scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16))
              plot4 <- ggplot(data = GR_AL$fluc) + 
                geom_vline(xintercept = as.Date(c("2020-03-12","2020-05-12", "2021-05-19")), color = "red", linetype = "dashed") + 
                geom_line(aes(x = days, y), color = "green4") + 
                geom_hline(yintercept = c(GR_AL$cv_sup, GR_AL$cv_inf), linetype = "dashed") + 
                ylab("Relative performance") + xlab(" ") + 
                ggtitle(bquote(.(modelo1)~"vs"~.(modelo2)~"."),
                        subtitle = bquote("AL scoring function. Risk level"~.(risklevel)~"%.")) + 
                theme(text = element_text(size = 16))
            }
          }
        }
      }
    }
  }
  plot1 + plot2 + plot3 + plot4
  ggsave(paste0(crypto,"_",modelo1, "_", modelo2, "_", round(risklevel,0), ".pdf"), width = 40, height = 21, units = "cm")
}

# BTC
methods = c("GARCH", "GAS", "FIGARCH", "CAViaRALD", "MAX", "AL_MSC", "AL_RSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "BTC", 2.5)
  }
}


methods = c("GARCH", "MSGARCH", "FIGARCH", "MED", "FZG_RSC", "AL_RSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "BTC", 5)
  }
}


# ETH

methods = c("GAS", "GJR", "GARCH", "MSGARCH", "Boot.", "FIGARCH", "NGARCH", "AVG", "MED", "FZG_MSC", "NZ_MSC", "AL_MSC", "AL_RSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "ETH", 2.5)
  }
}

methods = c("GAS", "GJR", "GARCH", "MSGARCH", "NGARCH", "CAViaR", "AVG", "MED", "FZG_MSC", "NZ_MSC", "FZG_RSC", "NZ_RSC", "AL_RSC")
for (i in 1:length(methods)) {
  for (j in (i + 1):length(methods)) {
    graficos_fluctuations(methods[i], methods[j], "ETH", 5)
  }
}

