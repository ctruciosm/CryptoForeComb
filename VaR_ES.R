################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### One-step-ahead forecast VaR/ES and in-sample VaR/ES estimation
#### using RobustGARCH, GAS and MSGARCH models
################################################################################## 

library(RobGARCHBoot)
library(GAS)
library(MSGARCH)
library(dplyr)
library(rugarch)

crytocurrency = "BTC/"              # Other options are: "BTC/", ETH/", "LTC/", and "XRP/"
alpha = c(0.01, 0.025, 0.05, 0.1)   # Risk levels
nmodels = 6                         # Number os models used

#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/")
#setwd("D:/Carlos/Research/Crypto_Forecast_Combination/CryptoForeComb/")  

if(crytocurrency == "BTC/"){
  crypto = read.csv("Data/BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
}

if(crytocurrency == "ETH/"){
  crypto = read.csv("Data/ETHUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2021-06-29")
}

if(crytocurrency == "LTC/"){
  crypto = read.csv("Data/LTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2021-06-29")
}

if(crytocurrency == "XRP/"){
  crypto =  read.csv("Data/XRPUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2021-06-29") 
}

OoS = 600
InS = dim(crypto)[1]-OoS

# Volatility models Specifications
GAS_Spec = UniGASSpec(Dist = "std", ScalingType = "Inv", GASPar = list(scale = TRUE))
MSGARC_Spec = CreateSpec(variance.spec = list(model = c("sGARCH","sGARCH")),
                         switch.spec = list(do.mix = FALSE),
                         distribution.spec = list(distribution = c("sstd", "sstd")))
MIXTURE_Spec = CreateSpec(variance.spec = list(model = c("sGARCH","sGARCH")),
                          switch.spec = list(do.mix = TRUE),
                          distribution.spec = list(distribution = c("sstd", "sstd")))
FIGARCH_Spec = ugarchspec(variance.model = list(model = 'fiGARCH', garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                          distribution = 'sstd')
AVGARCH_Spec = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                          distribution.model = "ged", fixed.pars = list(gamma1 = 0, delta = 1))


VaR = matrix(0,ncol= length(alpha)*nmodels+2, nrow = OoS)
ES = matrix(0,ncol= length(alpha)*nmodels, nrow = OoS)
nomes = c("GAS1", "GAS2", "GAS5", "GAS10",
          "MSGARCH1", "MSGARCH2", "MSGARCH5", "MSGARCH10",
          "Boot1", "Boot2", "Boot5", "Boot10",
          "FIGARCH1", "FIGARCH2", "FIGARCH5", "FIGARCH10",
          "AVGARCH1", "AVGARCH2", "AVGARCH5", "AVGARCH10",
          "MIXTURE1", "MIXTURE2", "MIXTURE5", "MIXTURE10",
          "OoS", "mu")
colnames(VaR) = nomes
colnames(ES) = nomes[1:(length(alpha)*nmodels)]
OoSret = mu = c()

inVaR1_Mix = inVaR2_Mix = inVaR5_Mix = inVaR10_Mix = inVaR1_AV = inVaR2_AV = inVaR5_AV = inVaR10_AV = inVaR1_FI = inVaR2_FI = inVaR5_FI = inVaR10_FI = inVaR1_MS = inVaR2_MS = inVaR5_MS = inVaR10_MS = inVaR1_GAS = inVaR2_GAS = inVaR5_GAS = inVaR10_GAS = inVaR1_Boot = inVaR2_Boot = inVaR5_Boot = inVaR10_Boot = matrix(0, ncol = OoS, nrow = InS)
inES1_Mix = inES2_Mix = inES5_Mix = inES10_Mix = inES1_AV = inES2_AV = inES5_AV = inES10_AV = inES1_FI = inES2_FI = inES5_FI = inES10_FI = inES1_MS = inES2_MS = inES5_MS = inES10_MS = inES1_GAS = inES2_GAS = inES5_GAS = inES10_GAS = inES1_Boot = inES2_Boot = inES5_Boot = inES10_Boot = matrix(0, ncol = OoS, nrow = InS)
vol_Mix = vol_AV = vol_FI = vol_Boot = vol_GAS = vol_MS = matrix(0, ncol = OoS, nrow = InS)

for (i in 1:OoS){
  print(paste(i,"of", OoS, "replications"))
# Rolling Windows 
  predailyreturns = crypto$ret[i:(InS+i-1)]
  VaR[i,length(alpha)*nmodels+1] = crypto$ret[InS+i]         # OoS return
  VaR[i,length(alpha)*nmodels+2] = mean(predailyreturns)     # InS mean
  dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
# Model Estimation
  GAS_fit = UniGASFit(GAS_Spec, dailyreturns, Compute.SE = FALSE)
  GAS_fore = UniGASFor(GAS_fit, H = 1)
  FIGARCH_fit = ugarchfit(FIGARCH_Spec, dailyreturns, solver = "hybrid", fit.control =  list(trunclag = 100))
  FIGARCH_fore = ugarchforecast(FIGARCH_fit, n.ahead = 1)
  AVGARCH_fit = ugarchfit(AVGARCH_Spec, dailyreturns, solver = "hybrid", solver.control = list(tol = 1e-10))
  AVGARCH_fore = ugarchforecast(AVGARCH_fit, n.ahead = 1, data = dailyreturns)  
  set.seed(i)
  MSGARCH_fit = FitMCMC(MSGARC_Spec,dailyreturns)
  MIXTURE_fit = FitMCMC(MIXTURE_Spec,dailyreturns)
  Boot = RobGARCHBoot(dailyreturns, n.boot = 2500, n.ahead = 1, ins = TRUE)
# In-sample volatilities
  vol_Boot[,i] = fitted_Vol(ROBUSTGARCH(dailyreturns),dailyreturns)[1:length(dailyreturns)]
  vol_GAS[,i] = sqrt(GAS_fit@GASDyn$mTheta[2,1:length(dailyreturns)])
  vol_MS[,i] = as.numeric(Volatility(MSGARCH_fit))  
  vol_FI[,i] = as.numeric(sigma(FIGARCH_fit)) 
  vol_AV[,i] = as.numeric(sigma(AVGARCH_fit)) 
  vol_Mix[,i] = as.numeric(Volatility(MIXTURE_fit)) 
# Computing VaR
  ## MSGARCH
  risk = Risk(MSGARCH_fit, alpha = alpha, nahead = 1)
  insampleRisk = Risk(MSGARCH_fit, alpha = alpha, do.its = TRUE)
  inVaR1_MS[,i] = insampleRisk$VaR[,1]
  inVaR2_MS[,i] = insampleRisk$VaR[,2]
  inVaR5_MS[,i] = insampleRisk$VaR[,3]
  inVaR10_MS[,i] = insampleRisk$VaR[,4]
  ## MIXTURE
  riskMix = Risk(MIXTURE_fit, alpha = alpha, nahead = 1)
  insampleRiskMix = Risk(MIXTURE_fit, alpha = alpha, do.its = TRUE)
  inVaR1_Mix[,i] = insampleRiskMix$VaR[,1]
  inVaR2_Mix[,i] = insampleRiskMix$VaR[,2]
  inVaR5_Mix[,i] = insampleRiskMix$VaR[,3]
  inVaR10_Mix[,i] = insampleRiskMix$VaR[,4] 
  ## GAS
  insampleGAS_VaR = quantile(GAS_fit, probs = alpha)
  inVaR1_GAS[,i] = insampleGAS_VaR[1:InS,1]
  inVaR2_GAS[,i] = insampleGAS_VaR[1:InS,2]
  inVaR5_GAS[,i] = insampleGAS_VaR[1:InS,3]
  inVaR10_GAS[,i] = insampleGAS_VaR[1:InS,4]
  ## Bootstrap
  inVaR1_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[1])
  inVaR2_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[2])
  inVaR5_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[3])
  inVaR10_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[4])
  BootVaR = quantile(Boot[[1]], prob = alpha)
  ## FIGARCH
  e_FI = as.numeric(dailyreturns/vol_FI[,i])
  param_FI = fitdist(distribution = "sstd", e_FI)$par
  insampleFI_VaR = matrix(as.vector(sigma(FIGARCH_fit)),ncol=1)%*%qdist(distribution = "sstd", alpha, 
                                                                       mu = ifelse(is.na(param_FI["mu"]), NULL, param_FI["mu"]),
                                                                       sigma = ifelse(is.na(param_FI["sigma"]), NULL, param_FI["sigma"]),
                                                                       skew = ifelse(is.na(param_FI["skew"]), NULL, param_FI["skew"]),
                                                                       shape = ifelse(is.na(param_FI["shape"]), NULL, param_FI["shape"]))
  inVaR1_FI[,i] = insampleFI_VaR[1:InS,1]
  inVaR2_FI[,i] = insampleFI_VaR[1:InS,2]
  inVaR5_FI[,i] = insampleFI_VaR[1:InS,3]
  inVaR10_FI[,i] = insampleFI_VaR[1:InS,4]
  VaR_FI = qdist(distribution = "sstd", alpha, 
                 mu = ifelse(is.na(param_FI["mu"]), NULL, param_FI["mu"]),
                 sigma = ifelse(is.na(param_FI["sigma"]), NULL, param_FI["sigma"]),
                 skew = ifelse(is.na(param_FI["skew"]), NULL, param_FI["skew"]),
                 shape = ifelse(is.na(param_FI["shape"]), NULL, param_FI["shape"]))*as.numeric(FIGARCH_fore@forecast$sigmaFor)
  ## AVGARCH
  e_AV = as.numeric(dailyreturns/vol_AV[,i])
  param_AV = fitdist(distribution = "ged", e_AV)$par
  insampleAV_VaR = matrix(as.vector(sigma(AVGARCH_fit)),ncol=1)%*%qdist(distribution = "ged", alpha, 
                                                                        mu = ifelse(is.na(param_AV["mu"]), NULL, param_AV["mu"]),
                                                                        sigma = ifelse(is.na(param_AV["sigma"]), NULL, param_AV["sigma"]),
                                                                        skew = ifelse(is.na(param_AV["skew"]), NULL, param_AV["skew"]),
                                                                        shape = ifelse(is.na(param_AV["shape"]), NULL, param_AV["shape"]))
  inVaR1_AV[,i] = insampleAV_VaR[1:InS,1]
  inVaR2_AV[,i] = insampleAV_VaR[1:InS,2]
  inVaR5_AV[,i] = insampleAV_VaR[1:InS,3]
  inVaR10_AV[,i] = insampleAV_VaR[1:InS,4]
  VaR_AV = qdist(distribution = "ged", alpha, 
                 mu = ifelse(is.na(param_AV["mu"]), NULL, param_AV["mu"]),
                 sigma = ifelse(is.na(param_AV["sigma"]), NULL, param_AV["sigma"]),
                 skew = ifelse(is.na(param_AV["skew"]), NULL, param_AV["skew"]),
                 shape = ifelse(is.na(param_AV["shape"]), NULL, param_AV["shape"]))*as.numeric(AVGARCH_fore@forecast$sigmaFor)
  
  VaR[i,1:(length(alpha)*nmodels)] = c(tail(insampleGAS_VaR,1), risk$VaR, BootVaR, VaR_FI, VaR_AV, riskMix$VaR)
# Computing ES
  ## FIGARCH
  f_FI = function(x) qdist(distribution = "sstd", p = x, 
                        mu = ifelse(is.na(param_FI["mu"]), NULL, param_FI["mu"]),
                        sigma = ifelse(is.na(param_FI["sigma"]), NULL, param_FI["sigma"]),
                        skew = ifelse(is.na(param_FI["skew"]), NULL, param_FI["skew"]),
                        shape = ifelse(is.na(param_FI["shape"]), NULL, param_FI["shape"]))
  ES_FI = c(as.numeric(FIGARCH_fore@forecast$sigmaFor)*integrate(f_FI, 0, 0.010)$value/0.010, 
            as.numeric(FIGARCH_fore@forecast$sigmaFor)*integrate(f_FI, 0, 0.025)$value/0.025, 
            as.numeric(FIGARCH_fore@forecast$sigmaFor)*integrate(f_FI, 0, 0.050)$value/0.050,
            as.numeric(FIGARCH_fore@forecast$sigmaFor)*integrate(f_FI, 0, 0.100)$value/0.100) 
  inES1_FI[,i] = as.numeric(sigma(FIGARCH_fit))*integrate(f_FI, 0, 0.010)$value/0.010
  inES2_FI[,i] = as.numeric(sigma(FIGARCH_fit))*integrate(f_FI, 0, 0.025)$value/0.025
  inES5_FI[,i] = as.numeric(sigma(FIGARCH_fit))*integrate(f_FI, 0, 0.050)$value/0.050
  inES10_FI[,i] = as.numeric(sigma(FIGARCH_fit))*integrate(f_FI,0, 0.100)$value/0.100
  ## AVGARCH
  f_AV = function(x) qdist(distribution = "ged", p = x, 
                           mu = ifelse(is.na(param_AV["mu"]), NULL, param_AV["mu"]),
                           sigma = ifelse(is.na(param_AV["sigma"]), NULL, param_AV["sigma"]),
                           skew = ifelse(is.na(param_AV["skew"]), NULL, param_AV["skew"]),
                           shape = ifelse(is.na(param_AV["shape"]), NULL, param_AV["shape"]))
  ES_AV = c(as.numeric(AVGARCH_fore@forecast$sigmaFor)*integrate(f_AV, 0, 0.010)$value/0.010, 
            as.numeric(AVGARCH_fore@forecast$sigmaFor)*integrate(f_AV, 0, 0.025)$value/0.025, 
            as.numeric(AVGARCH_fore@forecast$sigmaFor)*integrate(f_AV, 0, 0.050)$value/0.050,
            as.numeric(AVGARCH_fore@forecast$sigmaFor)*integrate(f_AV, 0, 0.100)$value/0.100) 
  inES1_AV[,i] = as.numeric(sigma(AVGARCH_fit))*integrate(f_AV, 0, 0.010)$value/0.010
  inES2_AV[,i] = as.numeric(sigma(AVGARCH_fit))*integrate(f_AV, 0, 0.025)$value/0.025
  inES5_AV[,i] = as.numeric(sigma(AVGARCH_fit))*integrate(f_AV, 0, 0.050)$value/0.050
  inES10_AV[,i] = as.numeric(sigma(AVGARCH_fit))*integrate(f_AV,0, 0.100)$value/0.100
  ## Bootstrap 
  BootES = c(mean(Boot[[1]][Boot[[1]]<BootVaR[1]]),
             mean(Boot[[1]][Boot[[1]]<BootVaR[2]]),
             mean(Boot[[1]][Boot[[1]]<BootVaR[3]]),
             mean(Boot[[1]][Boot[[1]]<BootVaR[4]]))
  for (j in 1:InS){
    inES1_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR1_Boot[j,i]])
    inES2_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR2_Boot[j,i]])
    inES5_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR5_Boot[j,i]])
    inES10_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR10_Boot[j,i]])
  }
  ## GAS
  insampleGAS_ES = ES(GAS_fit,probs = alpha)
  inES1_GAS[,i] = insampleGAS_ES[1:InS,1]
  inES2_GAS[,i] = insampleGAS_ES[1:InS,2]
  inES5_GAS[,i] = insampleGAS_ES[1:InS,3]
  inES10_GAS[,i] = insampleGAS_ES[1:InS,4]
  ## MSGARCH
  inES1_MS[,i] = insampleRisk$ES[,1]
  inES2_MS[,i] = insampleRisk$ES[,2]
  inES5_MS[,i] = insampleRisk$ES[,3]
  inES10_MS[,i] = insampleRisk$ES[,4]
  ## MIXTURE
  inES1_Mix[,i] = insampleRiskMix$ES[,1]
  inES2_Mix[,i] = insampleRiskMix$ES[,2]
  inES5_Mix[,i] = insampleRiskMix$ES[,3]
  inES10_Mix[,i] = insampleRiskMix$ES[,4]

  ES[i,] = c(tail(insampleGAS_ES,1), risk$ES, BootES, ES_FI, ES_AV, riskMix$ES)
}

# Creating subdirectory
if(dir.exists(crytocurrency)){
  unlink(crytocurrency, recursive = TRUE)
  dir.create(file.path(getwd(), crytocurrency), recursive = TRUE)
}else{
  dir.create(file.path(getwd(), crytocurrency), recursive = TRUE)
}

# Saving Oos VaR and ES
write.csv(VaR, paste0(crytocurrency,"VaR.csv"))
write.csv(ES, paste0(crytocurrency,"ES.csv"))

# Saving InS Volatilities
write.csv(vol_Boot, paste0(crytocurrency,"vol_Boot.csv"))
write.csv(vol_GAS, paste0(crytocurrency,"vol_GAS.csv"))
write.csv(vol_MS, paste0(crytocurrency,"vol_MS.csv"))
write.csv(vol_Mix, paste0(crytocurrency,"vol_Mix.csv"))
write.csv(vol_FI, paste0(crytocurrency,"vol_FI.csv"))
write.csv(vol_AV, paste0(crytocurrency,"vol_AV.csv"))

# Saving InS VaR and ES MSGARCH
write.csv(inVaR1_MS, paste0(crytocurrency,"inVaR1_MS.csv"))
write.csv(inVaR2_MS, paste0(crytocurrency,"inVaR2_MS.csv"))
write.csv(inVaR5_MS, paste0(crytocurrency,"inVaR5_MS.csv"))
write.csv(inVaR10_MS, paste0(crytocurrency,"inVaR10_MS.csv"))
write.csv(inES1_MS, paste0(crytocurrency,"inES1_MS.csv"))
write.csv(inES2_MS, paste0(crytocurrency,"inES2_MS.csv"))
write.csv(inES5_MS, paste0(crytocurrency,"inES5_MS.csv"))
write.csv(inES10_MS, paste0(crytocurrency,"inES10_MS.csv"))

# Saving InS VaR and ES MIXTURE
write.csv(inVaR1_Mix, paste0(crytocurrency,"inVaR1_Mix.csv"))
write.csv(inVaR2_Mix, paste0(crytocurrency,"inVaR2_Mix.csv"))
write.csv(inVaR5_Mix, paste0(crytocurrency,"inVaR5_Mix.csv"))
write.csv(inVaR10_Mix, paste0(crytocurrency,"inVaR10_Mix.csv"))
write.csv(inES1_Mix, paste0(crytocurrency,"inES1_Mix.csv"))
write.csv(inES2_Mix, paste0(crytocurrency,"inES2_Mix.csv"))
write.csv(inES5_Mix, paste0(crytocurrency,"inES5_Mix.csv"))
write.csv(inES10_Mix, paste0(crytocurrency,"inES10_Mix.csv"))

# Saving InS VaR and ES GAS
write.csv(inVaR1_GAS, paste0(crytocurrency,"inVaR1_GAS.csv"))
write.csv(inVaR2_GAS, paste0(crytocurrency,"inVaR2_GAS.csv"))
write.csv(inVaR5_GAS, paste0(crytocurrency,"inVaR5_GAS.csv"))
write.csv(inVaR10_GAS, paste0(crytocurrency,"inVaR10_GAS.csv"))
write.csv(inES1_GAS, paste0(crytocurrency,"inES1_GAS.csv"))
write.csv(inES2_GAS, paste0(crytocurrency,"inES2_GAS.csv"))
write.csv(inES5_GAS, paste0(crytocurrency,"inES5_GAS.csv"))
write.csv(inES10_GAS, paste0(crytocurrency,"inES10_GAS.csv"))

# Saving InS VaR and ES Bootstrap
write.csv(inVaR1_Boot, paste0(crytocurrency,"inVaR1_Boot.csv"))
write.csv(inVaR2_Boot, paste0(crytocurrency,"inVaR2_Boot.csv"))
write.csv(inVaR5_Boot, paste0(crytocurrency,"inVaR5_Boot.csv"))
write.csv(inVaR10_Boot, paste0(crytocurrency,"inVaR10_Boot.csv"))
write.csv(inES1_Boot, paste0(crytocurrency,"inES1_Boot.csv"))
write.csv(inES2_Boot, paste0(crytocurrency,"inES2_Boot.csv"))
write.csv(inES5_Boot, paste0(crytocurrency,"inES5_Boot.csv"))
write.csv(inES10_Boot, paste0(crytocurrency,"inES10_Boot.csv"))

# Saving InS VaR and ES FIGARCH
write.csv(inVaR1_FI, paste0(crytocurrency,"inVaR1_FI.csv"))
write.csv(inVaR2_FI, paste0(crytocurrency,"inVaR2_FI.csv"))
write.csv(inVaR5_FI, paste0(crytocurrency,"inVaR5_FI.csv"))
write.csv(inVaR10_FI, paste0(crytocurrency,"inVaR10_FI.csv"))
write.csv(inES1_FI, paste0(crytocurrency,"inES1_FI.csv"))
write.csv(inES2_FI, paste0(crytocurrency,"inES2_FI.csv"))
write.csv(inES5_FI, paste0(crytocurrency,"inES5_FI.csv"))
write.csv(inES10_FI, paste0(crytocurrency,"inES10_FI.csv"))

# Saving InS VaR and ES AVGARCH
write.csv(inVaR1_AV, paste0(crytocurrency,"inVaR1_AV.csv"))
write.csv(inVaR2_AV, paste0(crytocurrency,"inVaR2_AV.csv"))
write.csv(inVaR5_AV, paste0(crytocurrency,"inVaR5_AV.csv"))
write.csv(inVaR10_AV, paste0(crytocurrency,"inVaR10_AV.csv"))
write.csv(inES1_AV, paste0(crytocurrency,"inES1_AV.csv"))
write.csv(inES2_AV, paste0(crytocurrency,"inES2_AV.csv"))
write.csv(inES5_AV, paste0(crytocurrency,"inES5_AV.csv"))
write.csv(inES10_AV, paste0(crytocurrency,"inES10_AV.csv"))
