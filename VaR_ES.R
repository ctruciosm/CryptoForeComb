#### VaR and ES using RobustGARCH, GAS and MSGARCH


library(RobGARCHBoot)
library(GAS)
library(MSGARCH)
library(dplyr)

crytocurrency = "BTC/" # Other options are: "ETH/", "LTC/", and "XRP/"

#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/Data/")

if(crytocurrency == "BTC/"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-08-18")
}

if(crytocurrency == "ETH/"){
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-08-18")
}

if(crytocurrency == "LTC/"){
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2020-08-18")
}

if(crytocurrency == "XRP/"){
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2020-08-18") 
}



OoS = 365
InS = dim(crypto)[1]-OoS
alpha = c(0.01, 0.025, 0.05)

GAS_Spec = UniGASSpec(Dist = "std", ScalingType = "Inv", GASPar = list(scale = TRUE))
MSGARC_Spec = CreateSpec(variance.spec = list(model = c("gjrGARCH","gjrGARCH")),switch.spec = list(do.mix = FALSE),distribution.spec = list(distribution = c("sstd", "sstd")))

VaR = matrix(0,ncol= 11, nrow = OoS)
ES = matrix(0,ncol= 9, nrow = OoS)
nomes = c("GAS1", "GAS2", "GAS5", "MSGARCH1", "MSGARCH2", "MSGARCH5", "Boot1", "Boot2", "Boot5", "OoS", "mu")
colnames(VaR) = nomes
colnames(ES) = nomes[1:9]
OoSret = mu = c()

inVaR1_MS = inVaR2_MS = inVaR5_MS = inVaR1_GAS = inVaR2_GAS = inVaR5_GAS = inVaR1_Boot = inVaR2_Boot = inVaR5_Boot = matrix(0, ncol = OoS, nrow = InS)
inES1_MS = inES2_MS = inES5_MS = inES1_GAS = inES2_GAS = inES5_GAS = inES1_Boot = inES2_Boot = inES5_Boot = matrix(0, ncol = OoS, nrow = InS)

for (i in 1:OoS){
# Expanding Windows (because we have few observations)
  predailyreturns = crypto$ret[i:(InS+i-1)]
  VaR[i,11] = mean(predailyreturns)
  dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
# Model Estimation
  GAS_fit = UniGASFit(GAS_Spec, dailyreturns, Compute.SE = FALSE)
  GAS_fore = UniGASFor(GAS_fit, H = 1)
  set.seed(i)
  MSGARCH_fit = FitMCMC(MSGARC_Spec,dailyreturns)
  #Boot = RobGARCHBootParallel(dailyreturns, n.boot = 5000, n.ahead = 1, ins = TRUE)
  Boot = RobGARCHBoot(dailyreturns, n.boot = 5000, n.ahead = 1, ins = TRUE)
# Computing VaR
  risk = Risk(MSGARCH_fit, alpha = alpha, nahead = 1)
  insampleRisk = Risk(MSGARCH_fit, alpha = alpha, do.its = TRUE)
  inVaR1_MS[,i] = insampleRisk$VaR[,1]
  inVaR2_MS[,i] = insampleRisk$VaR[,2]
  inVaR5_MS[,i] = insampleRisk$VaR[,3]
  
  insampleGAS_VaR = quantile(GAS_fit, probs = alpha)
  inVaR1_GAS[,i] = insampleGAS_VaR[1:InS,1]
  inVaR2_GAS[,i] = insampleGAS_VaR[1:InS,2]
  inVaR5_GAS[,i] = insampleGAS_VaR[1:InS,3]
  
  inVaR1_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[1])
  inVaR2_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[2])
  inVaR5_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[3])
  
  BootVaR = quantile(Boot[[1]], prob = alpha)
  VaR[i,1:9] = c(tail(insampleGAS_VaR,1),risk$VaR, BootVaR)
# Computing ES
  BootES = c(mean(Boot[[1]][Boot[[1]]<BootVaR[1]]),mean(Boot[[1]][Boot[[1]]<BootVaR[2]]),mean(Boot[[1]][Boot[[1]]<BootVaR[3]]))

  insampleGAS_ES = ES(GAS_fit,probs = alpha)
  inES1_GAS[,i] = insampleGAS_ES[1:InS,1]
  inES2_GAS[,i] = insampleGAS_ES[1:InS,2]
  inES5_GAS[,i] = insampleGAS_ES[1:InS,3]
  
  inES1_MS[,i] = insampleRisk$ES[,1]
  inES2_MS[,i] = insampleRisk$ES[,2]
  inES5_MS[,i] = insampleRisk$ES[,3]
  
  
  for (j in 1:InS){
    inES1_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR1_Boot[j]])
    inES2_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR2_Boot[j]])
    inES5_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,]<inVaR5_Boot[j]])
  }
  
  ES[i,] = c(tail(insampleGAS_ES,1), risk$ES, BootES)
# Saving Out-of-Sample (OoS) returns
  VaR[i,10] = crypto$ret[InS+i]
  print(paste(i,"de", OoS, "replicacoes"))
}

write.csv(VaR, paste0(crytocurrency,"VaR.csv"))
write.csv(ES, paste0(crytocurrency,"ES.csv"))

write.csv(inVaR1_MS, paste0(crytocurrency,"inVaR1_MS.csv"))
write.csv(inVaR2_MS, paste0(crytocurrency,"inVaR2_MS.csv"))
write.csv(inVaR5_MS, paste0(crytocurrency,"inVaR5_MS.csv"))

write.csv(inES1_MS, paste0(crytocurrency,"inES1_MS.csv"))
write.csv(inES2_MS, paste0(crytocurrency,"inES2_MS.csv"))
write.csv(inES5_MS, paste0(crytocurrency,"inES5_MS.csv"))

write.csv(inVaR1_GAS, paste0(crytocurrency,"inVaR1_GAS.csv"))
write.csv(inVaR2_GAS, paste0(crytocurrency,"inVaR2_GAS.csv"))
write.csv(inVaR5_GAS, paste0(crytocurrency,"inVaR5_GAS.csv"))

write.csv(inES1_GAS, paste0(crytocurrency,"inES1_GAS.csv"))
write.csv(inES2_GAS, paste0(crytocurrency,"inES2_GAS.csv"))
write.csv(inES5_GAS, paste0(crytocurrency,"inES5_GAS.csv"))

write.csv(inVaR1_Boot, paste0(crytocurrency,"inVaR1_Boot.csv"))
write.csv(inVaR2_Boot, paste0(crytocurrency,"inVaR2_Boot.csv"))
write.csv(inVaR5_Boot, paste0(crytocurrency,"inVaR5_Boot.csv"))

write.csv(inES1_Boot, paste0(crytocurrency,"inES1_Boot.csv"))
write.csv(inES2_Boot, paste0(crytocurrency,"inES2_Boot.csv"))
write.csv(inES5_Boot, paste0(crytocurrency,"inES5_Boot.csv"))
