##################################################################################### 
##  Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies    ##
##        using Combinations based on Jump-Robust and Regime-Switching Models      ##
##  Authors: Carlos Trucíos and James W. Taylor
##################################################################################### 
### Implemented by Carlos Trucios
### Joint VaR and ES regression combining strategy (Dimitriadis and Bayer, 2019 EJS)
##################################################################################### 

crypto = "/LTC"  # Other options are "/BTC" and "/ETH"
setwd(paste0("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados",crypto))

library(dplyr)
library(esreg)

if(str_sub(getwd(), - 3, - 1)   == "BTC"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
}

if(str_sub(getwd(), - 3, - 1)   == "ETH"){
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
}

if(str_sub(getwd(), - 3, - 1)  == "LTC"){
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2021-06-29")
}

if(str_sub(getwd(), - 3, - 1)  == "XRP"){
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2021-06-29") 
}


# Load OoS Data
VaR = read.csv("VaR.csv")
ES = read.csv("ES.csv")
mu = VaR[,"mu"]
ret = VaR[,"OoS"]


VaR1 = VaR %>% dplyr::select(ends_with("1")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
VaR2 = VaR %>% dplyr::select(ends_with("2")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
VaR5 = VaR %>% dplyr::select(ends_with("5")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
VaR10 = VaR %>% dplyr::select(ends_with("10")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu

ES1 = ES %>% dplyr::select(ends_with("1")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
ES2 = ES %>% dplyr::select(ends_with("2")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
ES5 = ES %>% dplyr::select(ends_with("5")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu
ES10 = ES %>% dplyr::select(ends_with("10")) %>% dplyr::select(-contains("MIXTURE")) %>% as.matrix() + mu


# Load InS Data

inVaR1_Boot = read.csv("inVaR1_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_Boot = read.csv("inVaR2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_Boot = read.csv("inVaR5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_Boot = read.csv("inVaR10_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_Boot = read.csv("inES1_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_Boot = read.csv("inES2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_Boot = read.csv("inES5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_Boot = read.csv("inES10_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


inVaR1_GAS = read.csv("inVaR1_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_GAS = read.csv("inVaR2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_GAS = read.csv("inVaR5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_GAS = read.csv("inVaR10_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_GAS = read.csv("inES1_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_GAS = read.csv("inES2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_GAS = read.csv("inES5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_GAS = read.csv("inES10_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


inVaR1_MS = read.csv("inVaR1_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_MS = read.csv("inVaR2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_MS = read.csv("inVaR5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_MS = read.csv("inVaR10_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_MS = read.csv("inES1_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_MS = read.csv("inES2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_MS = read.csv("inES5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_MS = read.csv("inES10_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR1_FI = read.csv("inVaR1_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_FI = read.csv("inVaR2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_FI = read.csv("inVaR5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_FI = read.csv("inVaR10_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_FI = read.csv("inES1_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_FI = read.csv("inES2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_FI = read.csv("inES5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_FI = read.csv("inES10_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR1_AV = read.csv("inVaR1_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_AV = read.csv("inVaR2_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_AV = read.csv("inVaR5_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_AV = read.csv("inVaR10_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_AV = read.csv("inES1_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_AV = read.csv("inES2_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_AV = read.csv("inES5_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_AV = read.csv("inES10_AV.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


# Setup
a1 = 0.010
a2 = 0.025
a5 = 0.050
a10 = 0.10

OoS = length(ret)
InS = dim(crypto)[1]-OoS

## Formulas
formula1 =  Formula::as.Formula('r ~ VaRGAS1 + VaRMSGARCH1 + VaRBoot1 + VaRFIGARCH1 + VaRAVGARCH1 | ESGAS1 + ESMSGARCH1 + ESBoot1 + ESFIGARCH1 + ESAVGARCH1')
formula2 =  Formula::as.Formula('r ~ VaRGAS2 + VaRMSGARCH2 + VaRBoot2 + VaRFIGARCH2 + VaRAVGARCH2 | ESGAS2 + ESMSGARCH2 + ESBoot2 + ESFIGARCH2 + ESAVGARCH2')
formula5 =  Formula::as.Formula('r ~ VaRGAS5 + VaRMSGARCH5 + VaRBoot5 + VaRFIGARCH5 + VaRAVGARCH5 | ESGAS5 + ESMSGARCH5 + ESBoot5 + ESFIGARCH5 + ESAVGARCH5')
formula10 = Formula::as.Formula('r ~ VaRGAS10 + VaRMSGARCH10 + VaRBoot10 + VaRFIGARCH10 + VaRAVGARCH10 | ESGAS10 + ESMSGARCH10 + ESBoot10 + ESFIGARCH10 + ESAVGARCH10')

## OoS VaR and ES
### We can put any value on r, this value is not used in the predict function
df_OoS_01 = data.frame(0,VaR1, ES1)
colnames(df_OoS_01) = c("r","VaRGAS1", "VaRMSGARCH1", "VaRBoot1", "VaRFIGARCH1", "VaRAVGARCH1","ESGAS1", "ESMSGARCH1", "ESBoot1", "ESFIGARCH1", "ESAVGARCH1")
df_OoS_02 = data.frame(0,VaR2, ES2)
colnames(df_OoS_02) = c("r","VaRGAS2", "VaRMSGARCH2", "VaRBoot2", "VaRFIGARCH2", "VaRAVGARCH2","ESGAS2", "ESMSGARCH2", "ESBoot2", "ESFIGARCH2", "ESAVGARCH2")
df_OoS_05 = data.frame(0,VaR5, ES5)
colnames(df_OoS_05) = c("r","VaRGAS5", "VaRMSGARCH5", "VaRBoot5", "VaRFIGARCH5", "VaRAVGARCH5","ESGAS5", "ESMSGARCH5", "ESBoot5", "ESFIGARCH5", "ESAVGARCH5")
df_OoS_10 = data.frame(0,VaR10, ES10)
colnames(df_OoS_10) = c("r","VaRGAS10", "VaRMSGARCH10", "VaRBoot10", "VaRFIGARCH10", "VaRAVGARCH10","ESGAS10", "ESMSGARCH10", "ESBoot10", "ESFIGARCH10", "ESAVGARCH10")

VaR_JER= matrix(0, ncol = 4, nrow = OoS)
ES_JER = matrix(0, ncol = 4, nrow = OoS)
for (i in 1:OoS){
  print(i)
  df1 = data.frame(r =  crypto$ret[i:(InS+i-1)], VaRGAS1 = inVaR1_GAS[,i], VaRMSGARCH1 = inVaR1_MS[,i], VaRBoot1 = inVaR1_Boot[,i], VaRFIGARCH1 = inVaR1_FI[,i], VaRAVGARCH1 = inVaR1_AV[,i],
                   ESGAS1 = inES1_GAS[,i], ESMSGARCH1 = inES1_MS[,i], ESBoot1 = inES1_Boot[,i], ESFIGARCH1 = inES1_FI[,i], ESAVGARCH1 = inES1_AV[,i])
  df2 = data.frame(r =  crypto$ret[i:(InS+i-1)], VaRGAS2 = inVaR2_GAS[,i], VaRMSGARCH2 = inVaR2_MS[,i], VaRBoot2 = inVaR2_Boot[,i], VaRFIGARCH2 = inVaR2_FI[,i], VaRAVGARCH2 = inVaR2_AV[,i],
                   ESGAS2 = inES2_GAS[,i], ESMSGARCH2 = inES2_MS[,i], ESBoot2 = inES2_Boot[,i], ESFIGARCH2 = inES2_FI[,i], ESAVGARCH2 = inES2_AV[,i])
  df5 = data.frame(r =  crypto$ret[i:(InS+i-1)], VaRGAS5 = inVaR5_GAS[,i], VaRMSGARCH5 = inVaR5_MS[,i], VaRBoot5 = inVaR5_Boot[,i], VaRFIGARCH5 = inVaR5_FI[,i], VaRAVGARCH5 = inVaR5_AV[,i],
                   ESGAS5 = inES5_GAS[,i], ESMSGARCH5 = inES5_MS[,i], ESBoot5 = inES5_Boot[,i], ESFIGARCH5 = inES5_FI[,i], ESAVGARCH5 = inES5_AV[,i])
  df10= data.frame(r =  crypto$ret[i:(InS+i-1)], VaRGAS10 = inVaR10_GAS[,i], VaRMSGARCH10 = inVaR10_MS[,i], VaRBoot10 = inVaR10_Boot[,i], VaRFIGARCH10 = inVaR10_FI[,i], VaRAVGARCH10 = inVaR10_AV[,i],
                   ESGAS10 = inES10_GAS[,i], ESMSGARCH10 = inES10_MS[,i], ESBoot10 = inES10_Boot[,i], ESFIGARCH10 = inES10_FI[,i], ESAVGARCH10 = inES10_AV[,i]) 
  
  fit_01 = esreg(formula1, data = df1, alpha = a1)
  fit_02 = esreg(formula2, data = df2, alpha = a2)
  fit_05 = esreg(formula5, data = df5, alpha = a5)
  fit_10 = esreg(formula10, data = df10, alpha = a10)
  
  pred_comb_01 = predict(fit_01, df_OoS_01[i,])
  pred_comb_02 = predict(fit_02, df_OoS_02[i,])
  pred_comb_05 = predict(fit_05, df_OoS_05[i,])
  pred_comb_10 = predict(fit_10, df_OoS_10[i,])
  
  VaR_JER[i,] = c(pred_comb_01[1], pred_comb_02[1], pred_comb_05[1], pred_comb_10[1])
  ES_JER[i,] = c(pred_comb_01[2], pred_comb_02[2], pred_comb_05[2], pred_comb_10[2])   
}

colnames(VaR_JER) = c("VaR_JER1", "VaR_JER2", "VaR_JER5", "VaR_JER10")
colnames(ES_JER) = c("ES_JER1", "ES_JER2", "ES_JER5", "ES_JER10")

write.table(VaR_JER,"VaR_JER.csv")
write.table(ES_JER,"ES_JER.csv")
  
