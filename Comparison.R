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

setwd("./Data/BTC/")
p = 0.05
pMCS = 0.1
FullMCS = FALSE # or TRUE


if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
}

if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
}

if(str_sub(getwd(), - 3, - 1)   == "LTC"){
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2020-09-18")
}

if(str_sub(getwd(), - 3, - 1)   == "XRP"){
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2020-09-18") 
}


OoS = 365
InS = dim(crypto)[1]-OoS
crypto = crypto[(InS+1):(InS+OoS),]

CovidDay = which(crypto$date == "2020-03-12")
print(CovidDay)

# Setting 
mu = read.csv("VaR.csv")[,"mu"]
VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
ES = as.matrix(read.csv("ES.csv")[,-1]) + mu
ret = read.csv("VaR.csv")[,"OoS"]

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

names2 = c("Parametric2", "Bayesian2", "Bootstrap2", 
           "AVG2", "FGZ_RSC2", "NZ_RSC2", "AL_RSC2", "FGZ_MSC2", "NZ_MSC2", "AL_MSC2")

names5 = c("Parametric5", "Bayesian5", "Bootstrap5", 
           "AVG5", "FGZ_RSC5", "NZ_RSC5", "AL_RSC5", "FGZ_MSC5", "NZ_MSC5", "AL_MSC5")



#### VaR and ES 1%
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], 
             AL_AVG = VaR_AVG_AL[,1],
             FZG_RSC = VaR_RSC_FZG[,1], NZ_RSC = VaR_RSC_NZ[,1], AL_RSC = VaR_RSC_AL[,1],
             FZG_MSC = VaR_MSC_FZG[,1], NZ_MSC = VaR_MSC_NZ[,1], AL_MSC = VaR_MSC_AL[,1])

ES1 = cbind(ES[,c("GAS1","MSGARCH1","Boot1")], 
            AL_AVG = ES_AVG_AL[,1],
            FZG_RSC = ES_RSC_FZG[,1], NZ_RSC = ES_RSC_NZ[,1], AL_RSC = ES_RSC_AL[,1],
            FZG_MSC = ES_MSC_FZG[,1], NZ_MSC = ES_MSC_NZ[,1], AL_MSC = ES_MSC_AL[,1])

#### VaR and ES 2.5%
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], 
             AL_AVG = VaR_AVG_AL[,2],
             FZG_RSC = VaR_RSC_FZG[,2], NZ_RSC = VaR_RSC_NZ[,2], AL_RSC = VaR_RSC_AL[,2],
             FZG_MSC = VaR_MSC_FZG[,2], NZ_MSC = VaR_MSC_NZ[,2], AL_MSC = VaR_MSC_AL[,2])

ES2 = cbind(ES[,c("GAS2","MSGARCH2","Boot2")], 
            AL_AVG = ES_AVG_AL[,2],
            FZG_RSC = ES_RSC_FZG[,2], NZ_RSC = ES_RSC_NZ[,2], AL_RSC = ES_RSC_AL[,2],
            FZG_MSC = ES_MSC_FZG[,2], NZ_MSC = ES_MSC_NZ[,2], AL_MSC = ES_MSC_AL[,2])

#### VaR and ES 1%
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], 
             AL_AVG = VaR_AVG_AL[,3],
             FZG_RSC = VaR_RSC_FZG[,3], NZ_RSC = VaR_RSC_NZ[,3], AL_RSC = VaR_RSC_AL[,3],
             FZG_MSC = VaR_MSC_FZG[,3], NZ_MSC = VaR_MSC_NZ[,3], AL_MSC = VaR_MSC_AL[,3])

ES5 = cbind(ES[,c("GAS5","MSGARCH5","Boot5")], 
            AL_AVG = ES_AVG_AL[,3],
            FZG_RSC = ES_RSC_FZG[,3], NZ_RSC = ES_RSC_NZ[,3], AL_RSC = ES_RSC_AL[,3],
            FZG_MSC = ES_MSC_FZG[,3], NZ_MSC = ES_MSC_NZ[,3], AL_MSC = ES_MSC_AL[,3])

colnames(ES1) = colnames(VaR1) = names1
colnames(ES2) = colnames(VaR2) = names2
colnames(ES5) = colnames(VaR5) = names5


###########################################
#########   VaR Backtesting      ##########
###########################################
K = dim(VaR1)[2]

a1 = 0.010
a2 = 0.025
a5 = 0.050

BackVaRES1 = BackVaRES2 = BackVaRES5 = matrix(0,ncol = 18,nrow = K) 
colnames(BackVaRES1) = colnames(BackVaRES2) = colnames(BackVaRES5) = c("Hits", "UC", "CC", "DQ", "VQ", "MFE", "NZ", "ESR_1", "ESR_2" ,"ESR_3", "AQL", "AFZG", "ANZ", "AAL","AQL2", "AFZG2", "ANZ2", "AAL2")

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
                     suppressWarnings(esr_backtest_modified(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 1)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 2)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR1[,i], ES1[,i],alpha  = a1, B = 0, version = 3)$pvalue_twosided_asymptotic),
                     mean(QL(VaR1[,i],ret, alpha = a1)),
                     mean(FZG(VaR1[,i], ES1[,i], ret, alpha = a1)),
                     mean(NZ(VaR1[,i], ES1[,i], ret, alpha = a1)),
                     mean(AL(VaR1[,i], ES1[,i], ret, alpha = a1)),
                     mean(QL(VaR1[-CovidDay,i],ret[-CovidDay], alpha = a1)),
                     mean(FZG(VaR1[-CovidDay,i], ES1[-CovidDay,i], ret[-CovidDay], alpha = a1)),
                     mean(NZ(VaR1[-CovidDay,i], ES1[-CovidDay,i], ret[-CovidDay], alpha = a1)),
                     mean(AL(VaR1[-CovidDay,i], ES1[-CovidDay,i], ret[-CovidDay], alpha = a1)))
                     
                     
  
  BackVaRES2[i,] = c(mean(ret < VaR2[,i])*100,
                     BackT2$LRuc[2], BackT2$LRcc[2],BackT2$DQ$pvalue, VaR_VQR(ret, VaR2[,i], a2),
                     EBackT2$boot.p.value,
                     cc_backtest(ret, VaR2[,i], ES2[,i],  alpha  = a2)$pvalue_twosided_simple, 
                     suppressWarnings(esr_backtest_modified(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 1)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 2)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR2[,i], ES2[,i],alpha  = a2, B = 0, version = 3)$pvalue_twosided_asymptotic),
                     mean(QL(VaR2[,i],ret, alpha = a2)),
                     mean(FZG(VaR2[,i], ES2[,i], ret, alpha = a2)),
                     mean(NZ(VaR2[,i], ES2[,i], ret, alpha = a2)),
                     mean(AL(VaR2[,i], ES2[,i], ret, alpha = a2)),
                     mean(QL(VaR2[-CovidDay,i],ret[-CovidDay], alpha = a2)),
                     mean(FZG(VaR2[-CovidDay,i], ES2[-CovidDay,i], ret[-CovidDay], alpha = a2)),
                     mean(NZ(VaR2[-CovidDay,i], ES2[-CovidDay,i], ret[-CovidDay], alpha = a2)),
                     mean(AL(VaR2[-CovidDay,i], ES2[-CovidDay,i], ret[-CovidDay], alpha = a2)))
  
  BackVaRES5[i,] = c(mean(ret < VaR5[,i])*100, 
                     BackT5$LRuc[2], BackT5$LRcc[2],BackT5$DQ$pvalue, VaR_VQR(ret, VaR5[,i], a5),
                     EBackT5$boot.p.value,
                     cc_backtest(ret, VaR5[,i], ES5[,i],  alpha  = a5)$pvalue_twosided_simple, 
                     suppressWarnings(esr_backtest_modified(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 1)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 2)$pvalue_twosided_asymptotic),
                     suppressWarnings(esr_backtest_modified(ret, VaR5[,i], ES5[,i],alpha  = a5, B = 0, version = 3)$pvalue_twosided_asymptotic),
                     mean(QL(VaR5[,i],ret, alpha = a5)),
                     mean(FZG(VaR5[,i], ES5[,i], ret, alpha = a5)),
                     mean(NZ(VaR5[,i], ES5[,i], ret, alpha = a5)),
                     mean(AL(VaR5[,i], ES5[,i], ret, alpha = a5)),
                     mean(QL(VaR5[-CovidDay,i],ret[-CovidDay], alpha = a5)),
                     mean(FZG(VaR5[-CovidDay,i], ES5[-CovidDay,i], ret[-CovidDay], alpha = a5)),
                     mean(NZ(VaR5[-CovidDay,i], ES5[-CovidDay,i], ret[-CovidDay], alpha = a5)),
                     mean(AL(VaR5[-CovidDay,i], ES5[-CovidDay,i], ret[-CovidDay], alpha = a5)))
}


VaRES = rbind(BackVaRES1,BackVaRES2,BackVaRES5)
row.names(VaRES) = c(names1,names2,names5)
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





if(FullMCS == TRUE){
  Auxmatrix = matrix(0,ncol=length(row.names(VaRES_AUX)), nrow = 1)
  colnames(Auxmatrix) = row.names(VaRES_AUX)
  
  Aux1 = Auxmatrix %>% data.frame() %>% select(ends_with("1"))
  Aux2 = Auxmatrix %>% data.frame() %>% select(ends_with("2"))
  Aux5 = Auxmatrix %>% data.frame() %>% select(ends_with("5"))
  
  MCS1_MQL = rep(0,length(Aux1))
  MCS2_MQL = rep(0,length(Aux2))
  MCS5_MQL = rep(0,length(Aux5))
  MCS1_MFZG = rep(0,length(Aux1))
  MCS2_MFZG = rep(0,length(Aux2))
  MCS5_MFZG = rep(0,length(Aux5))
  MCS1_MNZ = rep(0,length(Aux1))
  MCS2_MNZ = rep(0,length(Aux2))
  MCS5_MNZ = rep(0,length(Aux5))
  MCS1_MAL = rep(0,length(Aux1))
  MCS2_MAL = rep(0,length(Aux2))
  MCS5_MAL = rep(0,length(Aux5))
if (ncol(Aux1)>1){
  MQL = QL(VaR1[,colnames(Aux1)],ret, alpha = a1)
  colnames(MQL) = colnames(Aux1)
  aux1_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS1_MQL[aux1_MQL] = 1

  MFZG = FZG(VaR1[,colnames(Aux1)],ES1[,colnames(Aux1)], ret, alpha = a1)
  colnames(MFZG) = colnames(Aux1)
  aux1_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS1_MFZG[aux1_MFZG] = 1 
  
  MNZ = NZ(VaR1[,colnames(Aux1)], ES1[,colnames(Aux1)], ret, alpha = a1)
  colnames(MNZ) = colnames(Aux1)
  aux1_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS1_MNZ[aux1_MNZ] = 1 
  
  MAL = AL(VaR1[,colnames(Aux1)], ES1[,colnames(Aux1)], ret, alpha = a1)
  colnames(MAL) = colnames(Aux1)
  aux1_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS1_MAL[aux1_MAL] = 1
} 


if (ncol(Aux2)>1){
  MQL = QL(VaR2[,colnames(Aux2)],ret, alpha = a2)
  colnames(MQL) = colnames(Aux2)
  aux2_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS2_MQL[aux2_MQL] = 1
  
  MFZG = FZG(VaR2[,colnames(Aux2)],ES2[,colnames(Aux2)], ret, alpha = a2)
  colnames(MFZG) = colnames(Aux2)
  aux2_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS2_MFZG[aux2_MFZG] = 1 
  
  MNZ = NZ(VaR2[,colnames(Aux2)], ES2[,colnames(Aux2)], ret, alpha = a2)
  colnames(MNZ) = colnames(Aux2)
  aux2_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS2_MNZ[aux2_MNZ] = 1 
  
  MAL = AL(VaR2[,colnames(Aux2)], ES2[,colnames(Aux2)], ret, alpha = a2)
  colnames(MAL) = colnames(Aux2)
  aux2_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS2_MAL[aux2_MAL] = 1
} 


if (ncol(Aux5)>1){
  MQL = QL(VaR5[,colnames(Aux5)],ret, alpha = a5)
  colnames(MQL) = colnames(Aux5)
  aux5_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS5_MQL[aux5_MQL] = 1
  
  MFZG = FZG(VaR5[,colnames(Aux5)],ES5[,colnames(Aux5)], ret, alpha = a5)
  colnames(MFZG) = colnames(Aux5)
  aux5_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS5_MFZG[aux5_MFZG] = 1 
  
  MNZ = NZ(VaR5[,colnames(Aux5)], ES5[,colnames(Aux5)], ret, alpha = a5)
  colnames(MNZ) = colnames(Aux5)
  aux5_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS5_MNZ[aux5_MNZ] = 1 
  
  MAL = AL(VaR5[,colnames(Aux5)], ES5[,colnames(Aux5)], ret, alpha = a5)
  colnames(MAL) = colnames(Aux5)
  aux5_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
  MCS5_MAL[aux5_MAL] = 1
} 
  MCCQL = c(MCS1_MQL,MCS2_MQL,MCS5_MQL)
  MCCFZG = c(MCS1_MFZG,MCS2_MFZG,MCS5_MFZG)
  MCCNZ = c(MCS1_MNZ,MCS2_MNZ,MCS5_MNZ)
  MCCAL = c(MCS1_MAL,MCS2_MAL,MCS5_MAL) 
  
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
  
} else {
  
  
  
  row.names(BackVaRES1) = names1
  row.names(BackVaRES2) = names2
  row.names(BackVaRES5) = names5
  
  BackVaRES1_short = BackVaRES1 %>% data.frame() %>% 
    filter(CC>p, VQ>p, MFE>p, NZ>p,ESR_3>p)
  
  BackVaRES2_short = BackVaRES2 %>% data.frame() %>%
    filter(CC>p, DQ>p, VQ>p, MFE>p, NZ>p, ESR_3>p)
  
  BackVaRES5_short = BackVaRES5 %>% data.frame() %>% 
    filter(CC>p, DQ>p, VQ>p, MFE>p, NZ>p,ESR_3>p)
  
  
  VaRES_short = rbind(BackVaRES1_short,BackVaRES2_short,BackVaRES5_short) %>% as.matrix()
  
  Auxmatrix = matrix(0,ncol=length(row.names(VaRES_short)), nrow = 1)
  colnames(Auxmatrix) = row.names(VaRES_short)
  
  Aux1 = Auxmatrix %>% data.frame() %>% select(ends_with("1"))
  Aux2 = Auxmatrix %>% data.frame() %>% select(ends_with("2"))
  Aux5 = Auxmatrix %>% data.frame() %>% select(ends_with("5"))
  
  MCS1_MQL = MCS2_MQL = MCS5_MQL = rep(0,dim(VaR1)[2])
  MCS1_MFZG = MCS2_MFZG = MCS5_MFZG =rep(0,dim(VaR1)[2])
  MCS1_MNZ = MCS2_MNZ = MCS5_MNZ = rep(0,dim(VaR1)[2])
  MCS1_MAL = MCS2_MAL = MCS5_MAL = rep(0,dim(VaR1)[2])

  if (ncol(Aux1)>1){
    MQL = QL(VaR1[,colnames(Aux1)],ret, alpha = a1)
    colnames(MQL) = colnames(Aux1)
    aux1_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS1_MQL = colnames(Aux1)[aux1_MQL]
    
    MFZG = FZG(VaR1[,colnames(Aux1)],ES1[,colnames(Aux1)], ret, alpha = a1)
    colnames(MFZG) = colnames(Aux1)
    aux1_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS1_MFZG = colnames(Aux1)[aux1_MFZG]

    MNZ = NZ(VaR1[,colnames(Aux1)], ES1[,colnames(Aux1)], ret, alpha = a1)
    colnames(MNZ) = colnames(Aux1)
    aux1_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS1_MNZ = colnames(Aux1)[aux1_MNZ]
    
    MAL = AL(VaR1[,colnames(Aux1)], ES1[,colnames(Aux1)], ret, alpha = a1)
    colnames(MAL) = colnames(Aux1)
    aux1_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS1_MAL = colnames(Aux1)[aux1_MAL]
  } else {
    if(ncol(Aux1) ==1){
      MCS1_MQL[1] = MCS1_MFZG[1] = MCS1_MNZ[1] = MCS1_MAL[1] = colnames(Aux1)
    }
  }
  
  
  if (ncol(Aux2)>1){
    MQL = QL(VaR2[,colnames(Aux2)],ret, alpha = a2)
    colnames(MQL) = colnames(Aux2)
    aux2_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS2_MQL = colnames(Aux2)[aux2_MQL]
    
    MFZG = FZG(VaR2[,colnames(Aux2)],ES2[,colnames(Aux2)], ret, alpha = a2)
    colnames(MFZG) = colnames(Aux2)
    aux2_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS2_MFZG = colnames(Aux2)[aux2_MFZG]
    
    MNZ = NZ(VaR2[,colnames(Aux2)], ES2[,colnames(Aux2)], ret, alpha = a2)
    colnames(MNZ) = colnames(Aux2)
    aux2_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS2_MNZ = colnames(Aux2)[aux2_MNZ]
    
    MAL = AL(VaR2[,colnames(Aux2)], ES2[,colnames(Aux2)], ret, alpha = a2)
    colnames(MAL) = colnames(Aux2)
    aux2_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS2_MAL = colnames(Aux2)[aux2_MAL]
  } else {
    if(ncol(Aux2) ==1){
      MCS2_MQL[1] = MCS2_MFZG[1] = MCS2_MNZ[1] = MCS2_MAL[1] = colnames(Aux2)
    }
  }
  
  
  
  if (ncol(Aux5)>1){
    MQL = QL(VaR5[,colnames(Aux5)],ret, alpha = a5)
    colnames(MQL) = colnames(Aux5)
    aux5_MQL = estMCS.quick(MQL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS5_MQL = colnames(Aux5)[aux5_MQL]
    
    MFZG = FZG(VaR5[,colnames(Aux5)],ES5[,colnames(Aux5)], ret, alpha = a5)
    colnames(MFZG) = colnames(Aux5)
    aux5_MFZG = estMCS.quick(MFZG, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS5_MFZG = colnames(Aux5)[aux5_MFZG]
    
    MNZ = NZ(VaR5[,colnames(Aux5)], ES5[,colnames(Aux5)], ret, alpha = a5)
    colnames(MNZ) = colnames(Aux5)
    aux5_MNZ = estMCS.quick(MNZ, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS5_MNZ = colnames(Aux5)[aux5_MNZ]
    
    MAL = AL(VaR5[,colnames(Aux5)], ES5[,colnames(Aux5)], ret, alpha = a5)
    colnames(MAL) = colnames(Aux5)
    aux5_MAL = estMCS.quick(MAL, test="t.range", B=5000, l=12, alpha = pMCS)
    MCS5_MAL = colnames(Aux5)[aux5_MAL]
  } else {
    if(ncol(Aux2) ==1){
      MCS5_MQL[1] = MCS5_MFZG[1] = MCS5_MNZ[1] = MCS5_MAL[1] = colnames(Aux5)
    }
  }
  
  VaRES_AUX = VaRES_AUX %>% data.frame() %>% mutate(MCCQL = ifelse(row.names(VaRES_AUX) %in% c(MCS1_MQL,MCS2_MQL,MCS5_MQL),1,0),
                                                    MCCFZG = ifelse(row.names(VaRES_AUX) %in% c(MCS1_MFZG,MCS2_MFZG,MCS5_MFZG),1,0), 
                                                    MCCNZ = ifelse(row.names(VaRES_AUX) %in% c(MCS1_MNZ,MCS2_MNZ,MCS5_MNZ),1,0),
                                                    MCCAL = ifelse(row.names(VaRES_AUX) %in% c(MCS1_MAL,MCS2_MAL,MCS5_MAL),1,0))
  row.names(VaRES_AUX) = row.names(VaRES)
  
 VaRES$MCCQL = VaRES_AUX$MCCQL 
 VaRES$MCCFZG = VaRES_AUX$MCCFZG 
 VaRES$MCCNZ  = VaRES_AUX$MCCNZ 
 VaRES$MCCAL = VaRES_AUX$MCCAL
 
 
 VaRES$AQL = ifelse(VaRES$MCCQL>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AQL,2),nsmall = 2)), format(round(VaRES$AQL,2),nsmall = 2))
 VaRES$AFZG  = ifelse(VaRES$MCCFZG>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AFZG,2),nsmall = 2)), format(round(VaRES$AFZG,2),nsmall = 2))
 VaRES$ANZ = ifelse(VaRES$MCCNZ>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$ANZ,2),nsmall = 2)), format(round(VaRES$ANZ,2),nsmall = 2))
 VaRES$AAL = ifelse(VaRES$MCCAL>pMCS,paste0('\\cellcolor{gray!25}',format(round(VaRES$AAL,2),nsmall = 2)), format(round(VaRES$AAL,2),nsmall = 2))
 
 
 VaRES$AQL2 = format(round(VaRES$AQL2,2),nsmall = 2)
 VaRES$AFZG2 = format(round(VaRES$AFZG2,2),nsmall = 2)
 VaRES$ANZ2 = format(round(VaRES$ANZ2,2),nsmall = 2)
 VaRES$AAL2 = format(round(VaRES$AAL2,2),nsmall = 2)
 
 
 VaRES = VaRES %>% select(-MCCQL,-MCCFZG,-MCCNZ,-MCCAL, -UC, -ESR_1, -ESR_2, -AQL2, -AFZG2, -ANZ2, -AAL2)
 
 
}





if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  Caption = "One-step-ahead VaR and ES backtesting for BTC at 1\\% (top panel), 2.5\\% (middle panel) and 5\\% (bottom panel) risk levels. Shaded cells in the calibration test indicate $p$-values larger than 0.01 while shaded cells in the scoring function stand for models in the MCS."
  print(xtable(VaRES, caption = Caption,  align = "l|cccc|ccc|cccc"), file = "VaRESBTC.tex", compress = FALSE)
}

if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  Caption = "One-step-ahead VaR and ES backtesting for ETH at 1\\% (top panel), 2.5\\% (middle panel) and 5\\% (bottom panel) risk levels. Shaded cells in the calibration test indicaate $p$-values larger than 0.01 while shaded cells in the scoring function stand for models in the MCS."
  print(xtable(VaRES, caption = Caption, align = "l|cccc|ccc|cccc"), file = "VaRESETH.tex", compress = FALSE)
}

if(str_sub(getwd(), - 3, - 1)  == "LTC"){
  Caption = "One-step-ahead VaR and ES backtesting for LTC at 1\\% (top panel), 2.5\\% (middle panel) and 5\\% (bottom panel) risk levels. Shaded cells in the calibration test indicaate $p$-values larger than 0.01 while shaded cells in the scoring function stand for models in the MCS."
  print(xtable(VaRES, caption = Caption, align = "l|cccc|ccc|cccc"), file = "VaRESLTC.tex", compress = FALSE)
}

if(str_sub(getwd(), - 3, - 1)  == "XRP"){
  Caption = "One-step-ahead VaR and ES backtesting for XRP at 1\\% (top panel), 2.5\\% (middle panel) and 5\\% (bottom panel) risk levels. Shaded cells in the calibration test indicaate $p$-values larger than 0.01 while shaded cells in the scoring function stand for models in the MCS."
  print(xtable(VaRES, caption = Caption,  align = "l|cccc|ccc|cccc"), file = "VaRESXRP.tex", compress = FALSE)
}

