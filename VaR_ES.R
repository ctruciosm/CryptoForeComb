#### VaR and ES using RobustGARCH, GAS and MSGARCH


library(RobGARCHBoot)
library(GAS)
library(MSGARCH)
library(dplyr)

setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/Dados")

crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-31")

#crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
#  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
#  dplyr::select(date, ret) %>% filter(date > "2017-08-31")


#crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
#  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
#  dplyr::select(date, ret) %>% filter(date > "2017-12-13")

#crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
#  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
#  dplyr::select(date, ret) %>% filter(date > "2018-05-04") 




OoS = 365
InS = dim(crypto)[1]-OoS
alpha = c(0.01, 0.025, 0.05)

GAS_Spec = UniGASSpec(Dist = "std", GASPar = list(scale = TRUE))
MSGARC_Spec = CreateSpec(variance.spec = list(model = c("gjrGARCH","gjrGARCH")),switch.spec = list(do.mix = FALSE),distribution.spec = list(distribution = c("sstd", "sstd")))

VaR = matrix(0,ncol= 11, nrow = OoS)
ES = matrix(0,ncol= 9, nrow = OoS)
nomes = c("GAS1", "GAS2", "GAS5", "MSGARCH1", "MSGARCH2", "MSGARCH5", "Boot1", "Boot2", "Boot5", "OoS", "mu")
colnames(VaR) = nomes
colnames(ES) = nomes[1:9]
OoSret = mu = c()

for (i in 1:OoS){
# Expanding Windows (because we have few observations)
  predailyreturns = crypto$ret[1:(InS+i-1)]
  VaR[i,11] = mean(predailyreturns)
  dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
# Model Estimation
  GAS_fit = UniGASFit(GAS_Spec, dailyreturns, Compute.SE = FALSE)
  GAS_fore = UniGASFor(GAS_fit, H = 1)
  set.seed(i)
  MSGARCH_fit = FitMCMC(MSGARC_Spec,dailyreturns)
  Boot = RobGARCHBootParallel(dailyreturns, n.boot = 5000, n.ahead = 1)
# Computing VaR
  risk = Risk(MSGARCH_fit, alpha = alpha, nahead = 1)
  BootVaR = quantile(Boot[[1]], prob = alpha)
  VaR[i,1:9] = c(quantile(GAS_fore, probs = alpha),risk$VaR, BootVaR)
# Computing ES
  BootES = c(mean(Boot[[1]][Boot[[1]]<BootVaR[1]]),mean(Boot[[1]][Boot[[1]]<BootVaR[2]]),mean(Boot[[1]][Boot[[1]]<BootVaR[3]]))
  ES[i,] = c(ES(GAS_fore,probs = alpha), risk$ES, BootES)
# Saving Out-of-Sample (OoS) returns
  VaR[i,10] = crypto$ret[InS+i]
  print(paste(i,"de", OoS, "replicacoes"))
}

write.csv(VaR, "VaR.csv")
write.csv(ES, "ES.csv")

