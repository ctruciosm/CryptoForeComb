################################################################################## 
## Paper: A Comparison of Methods for Forecasting Value-at-Risk and Expected    ##
##        Shortfall of Cryptocurrencies.                                        ##
## By Carlos Trucíos and James W. Taylor                                        ##
################################################################################## 
#### Implemented by Carlos Trucios
#### One-step-ahead forecast VaR/ES and in-sample VaR/ES estimation
################################################################################## 
rm(list = ls())
library(RobGARCHBoot)
library(GAS)
library(MSGARCH)
library(dplyr)
library(rugarch)
source("CAViaR.R")


crytocurrency = "ETH/"              # Other options are: "BTC/", ETH/" and "LTC/"
alpha = c(0.025, 0.05)              # Risk levels
nmodels = 8                         # Number os models used
end_date = "2021-08-14"

if (crytocurrency == "BTC/") {
  crypto = read.csv("Data/BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

if (crytocurrency == "ETH/") {
  crypto = read.csv("Data/ETHUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

OoS = 600
InS = dim(crypto)[1] - OoS

# Volatility models Specifications
# ast1 is another parametrization of sstd. See Zhu et al. (2010)
GAS_Spec = UniGASSpec(Dist = "sstd", GASPar = list(scale = TRUE))
MSGARC_Spec = CreateSpec(variance.spec = list(model = c("sGARCH","sGARCH")),
                         switch.spec = list(do.mix = FALSE),
                         distribution.spec = list(distribution = c("sstd", "sstd")))
FIGARCH_Spec = ugarchspec(variance.model = list(model = 'fiGARCH', garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0,0), include.mean = FALSE), 
                          distribution = 'sstd')
CSGARCH_Spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                          distribution.model = "sstd")

VaR = matrix(0,ncol = length(alpha)*nmodels + 2, nrow = OoS)
ES = matrix(0,ncol = length(alpha)*nmodels, nrow = OoS)
nomes = c("GAS2", "GAS5",
          "MSGARCH2", "MSGARCH5",
          "Boot2", "Boot5",
          "FIGARCH2", "FIGARCH5",
          "CSGARCH2", "CSGARCH5",
          "CAViaR2", "CAViaR5",
          "CAViaREVT2", "CAViaREVT5",
          "CAViaRALD2", "CAViaRALD5",
          "OoS", "mu")
colnames(VaR) = nomes
colnames(ES) = nomes[1:(length(alpha)*nmodels)]
OoSret = mu = c()

inVaR2_CS = inVaR5_CS = inVaR2_FI = inVaR5_FI = inVaR2_MS = inVaR5_MS = inVaR2_GAS = inVaR5_GAS = inVaR2_Boot = inVaR5_Boot =  inVaR2_CAViaR =  inVaR5_CAViaR = inVaR2_CAViaREVT =  inVaR5_CAViaREVT = inVaR2_CAViaRALD =  inVaR5_CAViaRALD = matrix(0, ncol = OoS, nrow = InS)
inES2_CS = inES5_CS = inES2_FI = inES5_FI = inES2_MS = inES5_MS = inES2_GAS = inES5_GAS = inES2_Boot = inES5_Boot =  inES2_CAViaR =  inES5_CAViaR = inES2_CAViaREVT =  inES5_CAViaREVT = inES2_CAViaRALD =  inES5_CAViaRALD = matrix(0, ncol = OoS, nrow = InS)
caviar2 = caviar5 = caviar_evt2 = caviar_evt5 = NULL
vol_CS = vol_FI = vol_Boot = vol_GAS = vol_MS = matrix(0, ncol = OoS, nrow = InS)

for (i in 1:OoS) {
  print(paste(i,"of", OoS, "replications"))
# Rolling Windows 
  predailyreturns = crypto$ret[i:(InS + i - 1)]
  VaR[i,length(alpha)*nmodels + 1] = crypto$ret[InS + i]         # OoS return
  VaR[i,length(alpha)*nmodels + 2] = mean(predailyreturns)       # InS mean
  dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
# Model Estimation
  GAS_fit = UniGASFit(GAS_Spec, dailyreturns, Compute.SE = FALSE)
  GAS_fore = UniGASFor(GAS_fit, H = 1)
  FIGARCH_fit = ugarchfit(FIGARCH_Spec, dailyreturns, solver = "hybrid", fit.control =  list(trunclag = 100))
  FIGARCH_fore = ugarchforecast(FIGARCH_fit, n.ahead = 1)
  CSGARCH_fit = ugarchfit(CSGARCH_Spec, dailyreturns, solver = "hybrid", solver.control = list(tol = 1e-10))
  CSGARCH_fore = ugarchforecast(CSGARCH_fit, n.ahead = 1, data = dailyreturns)  
  set.seed(i)
  MSGARCH_fit = FitMCMC(MSGARC_Spec,dailyreturns)
  Boot = RobGARCHBoot(dailyreturns, n.boot = 3000, n.ahead = 1, ins = TRUE)
  # In-sample volatilities for FIGARCH and CSGARCH
  vol_FI[,i] = as.numeric(sigma(FIGARCH_fit)) 
  vol_CS[,i] = as.numeric(sigma(CSGARCH_fit)) 
  # Computing VaR
  ## MSGARCH
  risk = Risk(MSGARCH_fit, alpha = alpha, nahead = 1)
  insampleRisk = Risk(MSGARCH_fit, alpha = alpha, do.its = TRUE)
  inVaR2_MS[,i] = insampleRisk$VaR[,1]
  inVaR5_MS[,i] = insampleRisk$VaR[,2]
  ## GAS
  insampleGAS_VaR = quantile(GAS_fit, probs = alpha)
  inVaR2_GAS[,i] = insampleGAS_VaR[1:InS,1]
  inVaR5_GAS[,i] = insampleGAS_VaR[1:InS,2]
  ## Bootstrap
  inVaR2_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[1])
  inVaR5_Boot[,i] = apply(Boot[[3]],1,quantile, prob = alpha[2])
  BootVaR = quantile(Boot[[1]], prob = alpha)
  ## FIGARCH
  inVaR2_FI[,i] = qdist(distribution = "sstd", alpha[1], mu = 0, sigma = as.vector(sigma(FIGARCH_fit)), skew = coef(FIGARCH_fit)["skew"], shape = coef(FIGARCH_fit)["shape"])
  inVaR5_FI[,i] = qdist(distribution = "sstd", alpha[2], mu = 0, sigma = as.vector(sigma(FIGARCH_fit)), skew = coef(FIGARCH_fit)["skew"], shape = coef(FIGARCH_fit)["shape"])
  VaR_FI = qdist(distribution = "sstd", alpha, mu = 0, sigma = as.numeric(FIGARCH_fore@forecast$sigmaFor), skew = coef(FIGARCH_fit)["skew"], shape = coef(FIGARCH_fit)["shape"])
  ## CSGARCH
  inVaR2_CS[,i] = qdist(distribution = "sstd", alpha[1], mu = 0, sigma = as.vector(sigma(CSGARCH_fit)), skew = coef(CSGARCH_fit)["skew"], shape = coef(CSGARCH_fit)["shape"])
  inVaR5_CS[,i] = qdist(distribution = "sstd", alpha[2], mu = 0, sigma = as.vector(sigma(CSGARCH_fit)), skew = coef(CSGARCH_fit)["skew"], shape = coef(CSGARCH_fit)["shape"])
  VaR_CS = qdist(distribution = "sstd", alpha, mu = 0, sigma = as.numeric(CSGARCH_fore@forecast$sigmaFor), skew = coef(CSGARCH_fit)["skew"], shape = coef(CSGARCH_fit)["shape"])
  ## Quantile Methods
  caviar2 = CAViaR_reg(dailyreturns, risklevel = alpha[1], type = "sym_abs", par_ini = caviar2[[2]])
  caviar5 = CAViaR_reg(dailyreturns, risklevel = alpha[2], type = "sym_abs", par_ini = caviar5[[2]])
  caviar_evt2 = CAViaR_EVT(dailyreturns, risklevel = alpha[1], type = "sym_abs", par_ini = caviar_evt2[[2]])
  caviar_evt5 = CAViaR_EVT(dailyreturns, risklevel = alpha[2], type = "sym_abs", par_ini = caviar_evt5[[2]])
  caviar_ald2 = ALD(dailyreturns, risklevel = alpha[1], type = "sym_abs", caviar_params = caviar2[[2]])
  caviar_ald5 = ALD(dailyreturns, risklevel = alpha[2], type = "sym_abs", caviar_params = caviar5[[2]])
  inVaR2_CAViaR[,i] = caviar2[[1]][1:InS,1]
  inVaR5_CAViaR[,i] = caviar5[[1]][1:InS,1]
  inVaR2_CAViaREVT[,i] = caviar_evt2[[1]][1:InS,1]
  inVaR5_CAViaREVT[,i] = caviar_evt5[[1]][1:InS,1]
  inVaR2_CAViaRALD[,i] = caviar_ald2[[1]][1:InS,1]
  inVaR5_CAViaRALD[,i] = caviar_ald5[[1]][1:InS,1]
  
  VaR[i,1:(length(alpha)*nmodels)] = c(tail(insampleGAS_VaR,1), risk$VaR, BootVaR, VaR_FI, VaR_CS, 
                                       caviar2[[1]][InS + 1,1], caviar5[[1]][InS + 1,1], 
                                       caviar_evt2[[1]][InS + 1,1], caviar_evt5[[1]][InS + 1,1],
                                       caviar_ald2[[1]][InS + 1,1], caviar_ald5[[1]][InS + 1,1])
  # Computing ES
  ## FIGARCH
  f_FI = function(x, sigma_) x*ddist(distribution = "sstd", y = x, 
                           mu = 0, sigma = sigma_, skew = coef(FIGARCH_fit)["skew"],
                           shape = coef(FIGARCH_fit)["shape"])
  ES_FI = c(integrate(f_FI, -Inf, VaR_FI[1], sigma_ = as.numeric(FIGARCH_fore@forecast$sigmaFor))$value/0.025, 
            integrate(f_FI, -Inf, VaR_FI[2], sigma_ = as.numeric(FIGARCH_fore@forecast$sigmaFor))$value/0.050)  
  for (j in 1:InS) {
    inES2_FI[j,i] = integrate(f_FI, -Inf, inVaR2_FI[j,i], sigma_ = vol_FI[j,i])$value/0.025
    inES5_FI[j,i] = integrate(f_FI, -Inf, inVaR5_FI[j,i], sigma_ = vol_FI[j,i])$value/0.050
  }
  ## CSGARCH
  f_CS = function(x, sigma_) x*ddist(distribution = "sstd", y = x, 
                           mu = 0, sigma = sigma_, skew = coef(CSGARCH_fit)["skew"],
                           shape = coef(CSGARCH_fit)["shape"])
  ES_CS = c(integrate(f_CS, -Inf, VaR_CS[1], sigma_ = as.numeric(CSGARCH_fore@forecast$sigmaFor))$value/0.025, 
            integrate(f_CS, -Inf, VaR_CS[2], sigma_ = as.numeric(CSGARCH_fore@forecast$sigmaFor))$value/0.050)  
  for (j in 1:InS) {
    inES2_CS[j,i] = integrate(f_CS, -Inf, inVaR2_CS[j,i], sigma_ = vol_CS[j,i])$value/0.025
    inES5_CS[j,i] = integrate(f_CS, -Inf, inVaR5_CS[j,i], sigma_ = vol_CS[j,i])$value/0.050
  }
  ## Bootstrap 
  BootES = c(mean(Boot[[1]][Boot[[1]] <= BootVaR[1]]),
             mean(Boot[[1]][Boot[[1]] <= BootVaR[2]]))
  for (j in 1:InS) {
    inES2_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,] <= inVaR2_Boot[j,i]])
    inES5_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,] <= inVaR5_Boot[j,i]])
  }
  ## GAS
  insampleGAS_ES = ES(GAS_fit,probs = alpha)
  inES2_GAS[,i] = insampleGAS_ES[1:InS,1]
  inES5_GAS[,i] = insampleGAS_ES[1:InS,2]
  ## MSGARCH
  inES2_MS[,i] = insampleRisk$ES[,1]
  inES5_MS[,i] = insampleRisk$ES[,2]
  ## Quantile methods
  inES2_CAViaR[,i] = caviar2[[1]][1:InS,2]
  inES5_CAViaR[,i] = caviar5[[1]][1:InS,2]
  inES2_CAViaREVT[,i] = caviar_evt2[[1]][1:InS,2]
  inES5_CAViaREVT[,i] = caviar_evt5[[1]][1:InS,2]
  inES2_CAViaRALD[,i] = caviar_ald2[[1]][1:InS,2]
  inES5_CAViaRALD[,i] = caviar_ald5[[1]][1:InS,2]
  

  ES[i,] = c(tail(insampleGAS_ES,1), risk$ES, BootES, ES_FI, ES_CS,
             caviar2[[1]][InS + 1,2], caviar5[[1]][InS + 1,2],
             caviar_evt2[[1]][InS + 1,2], caviar_evt5[[1]][InS + 1,2],
             caviar_ald2[[1]][InS + 1,2], caviar_ald5[[1]][InS + 1,2])
}

# Creating subdirectory
if (dir.exists(crytocurrency)) {
  unlink(crytocurrency, recursive = TRUE)
  dir.create(file.path(getwd(), crytocurrency), recursive = TRUE)
} else{
  dir.create(file.path(getwd(), crytocurrency), recursive = TRUE)
}

# Saving Oos VaR and ES
write.csv(VaR, paste0(crytocurrency,"VaR.csv"))
write.csv(ES, paste0(crytocurrency,"ES.csv"))

# Saving InS VaR and ES MSGARCH
write.csv(inVaR2_MS, paste0(crytocurrency,"inVaR2_MS.csv"))
write.csv(inVaR5_MS, paste0(crytocurrency,"inVaR5_MS.csv"))
write.csv(inES2_MS, paste0(crytocurrency,"inES2_MS.csv"))
write.csv(inES5_MS, paste0(crytocurrency,"inES5_MS.csv"))

# Saving InS VaR and ES GAS
write.csv(inVaR2_GAS, paste0(crytocurrency,"inVaR2_GAS.csv"))
write.csv(inVaR5_GAS, paste0(crytocurrency,"inVaR5_GAS.csv"))
write.csv(inES2_GAS, paste0(crytocurrency,"inES2_GAS.csv"))
write.csv(inES5_GAS, paste0(crytocurrency,"inES5_GAS.csv"))

# Saving InS VaR and ES Bootstrap
write.csv(inVaR2_Boot, paste0(crytocurrency,"inVaR2_Boot.csv"))
write.csv(inVaR5_Boot, paste0(crytocurrency,"inVaR5_Boot.csv"))
write.csv(inES2_Boot, paste0(crytocurrency,"inES2_Boot.csv"))
write.csv(inES5_Boot, paste0(crytocurrency,"inES5_Boot.csv"))

# Saving InS VaR and ES FIGARCH
write.csv(inVaR2_FI, paste0(crytocurrency,"inVaR2_FI.csv"))
write.csv(inVaR5_FI, paste0(crytocurrency,"inVaR5_FI.csv"))
write.csv(inES2_FI, paste0(crytocurrency,"inES2_FI.csv"))
write.csv(inES5_FI, paste0(crytocurrency,"inES5_FI.csv"))

# Saving InS VaR and ES AVGARCH
write.csv(inVaR2_CS, paste0(crytocurrency,"inVaR2_CS.csv"))
write.csv(inVaR5_CS, paste0(crytocurrency,"inVaR5_CS.csv"))
write.csv(inES2_CS, paste0(crytocurrency,"inES2_CS.csv"))
write.csv(inES5_CS, paste0(crytocurrency,"inES5_CS.csv"))

# Saving InS VaR and ES Quantile methods
write.csv(inVaR2_CAViaR, paste0(crytocurrency,"inVaR2_CAViaR.csv"))
write.csv(inVaR5_CAViaR, paste0(crytocurrency,"inVaR5_CAViaR.csv"))
write.csv(inES2_CAViaR, paste0(crytocurrency,"inES2_CAViaR.csv"))
write.csv(inES5_CAViaR, paste0(crytocurrency,"inES2_CAViaR.csv"))

write.csv(inVaR2_CAViaREVT, paste0(crytocurrency,"inVaR2_CAViaREVT.csv"))
write.csv(inVaR5_CAViaREVT, paste0(crytocurrency,"inVaR5_CAViaREVT.csv"))
write.csv(inES2_CAViaREVT, paste0(crytocurrency,"inES2_CAViaREVT.csv"))
write.csv(inES5_CAViaREVT, paste0(crytocurrency,"inES5_CAViaREVT.csv"))

write.csv(inVaR2_CAViaRALD, paste0(crytocurrency,"inVaR2_CAViaRALD.csv"))
write.csv(inVaR5_CAViaRALD, paste0(crytocurrency,"inVaR5_CAViaRALD.csv"))
write.csv(inES2_CAViaRALD, paste0(crytocurrency,"inES2_CAViaRALD.csv"))
write.csv(inES5_CAViaRALD, paste0(crytocurrency,"inES5_CAViaRALD.csv"))