################################################################################## 
## Paper: A Comparison of Methods for Forecasting Value-at-Risk and Expected    ##
##        Shortfall of Cryptocurrencies.                                        ##
## By Carlos Trucíos and James W. Taylor                                        ##
################################################################################## 
#### Implemented by Carlos Trucios
#### One-step-ahead forecast VaR/ES and in-sample VaR/ES estimation
################################################################################## 
#setwd("/Volumes/CTRUCIOS_SD/UNICAMP/Ongoing Research/ForecastCombinationCrypto/CryptoForeComb/")
rm(list = ls())
library(RobGARCHBoot)
library(GAS)
library(MSGARCH)
library(dplyr)
library(rugarch)
source("CAViaR.R")


crytocurrency = "ETH/"              # "BTC/" or "ETH/"
alpha = c(0.025, 0.05)              # Risk levels
nmodels = 10                        # Number of models used
end_date = "2022-07-23"
OoS = 800

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
GARCH_Spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                          distribution = 'sstd')
GJR_Spec = ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                        distribution = 'sstd')
NGARCH_Spec = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "NAGARCH"),
                          mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                          distribution.model = "sstd")
AR1_Spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 0)), 
                     mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), 
                     distribution.model = "sstd")
AR2_Spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 0)), 
                     mean.model = list(armaOrder = c(2, 0), include.mean = FALSE), 
                     distribution.model = "sstd")

VaR = matrix(0,ncol = length(alpha)*nmodels + 2, nrow = OoS)
ES = matrix(0,ncol = length(alpha)*nmodels, nrow = OoS)
nomes = c("GARCH2", "GARCH5",
          "GJR2", "GJR5",
          "GAS2", "GAS5",
          "MSGARCH2", "MSGARCH5",
          "Boot2", "Boot5",
          "FIGARCH2", "FIGARCH5",
          "NGARCH2", "NGARCH5",
          "CAViaR2", "CAViaR5",
          "CAViaREVT2", "CAViaREVT5",
          "CAViaRALD2", "CAViaRALD5",
          "OoS", "mu")
colnames(VaR) = nomes
colnames(ES) = nomes[1:(length(alpha)*nmodels)]
OoSret = mu = c()

inVaR2_GJR = inVaR5_GJR = inVaR2_GARCH = inVaR5_GARCH = inVaR2_NG = inVaR5_NG = inVaR2_FI = inVaR5_FI = inVaR2_MS = inVaR5_MS = inVaR2_GAS = inVaR5_GAS = inVaR2_Boot = inVaR5_Boot =  inVaR2_CAViaR =  inVaR5_CAViaR = inVaR2_CAViaREVT =  inVaR5_CAViaREVT = inVaR2_CAViaRALD =  inVaR5_CAViaRALD = matrix(0, ncol = OoS, nrow = InS)
inES2_GJR = inES5_GJR = inES2_GARCH = inES5_GARCH = inES2_NG = inES5_NG = inES2_FI = inES5_FI = inES2_MS = inES5_MS = inES2_GAS = inES5_GAS = inES2_Boot = inES5_Boot =  inES2_CAViaR =  inES5_CAViaR = inES2_CAViaREVT =  inES5_CAViaREVT = inES2_CAViaRALD =  inES5_CAViaRALD = matrix(0, ncol = OoS, nrow = InS)
vol_GJR = vol_GARCH = vol_NG = vol_FI = vol_Boot = vol_GAS = vol_MS = matrix(0, ncol = OoS, nrow = InS)
caviar2 = caviar5 = caviar_evt2 = caviar_evt5 = NULL

see_ar <- c()
for (i in 1:OoS) {
  print(paste(i,"of", OoS, "replications"))
# Rolling Windows 
  predailyreturns = crypto$ret[i:(InS + i - 1)]
  VaR[i,length(alpha)*nmodels + 1] = crypto$ret[InS + i]         # OoS return
  
  AR1_fit = ugarchfit(AR1_Spec, predailyreturns, solver = "hybrid")
  AR2_fit = ugarchfit(AR2_Spec, predailyreturns, solver = "hybrid")
  
  if (infocriteria(AR1_fit)[1] <= infocriteria(AR2_fit)[1]) {
    AR_fore = ugarchforecast(AR1_fit, n.ahead = 1)
    dailyreturns = residuals(AR1_fit)
    see_ar[i] <- 1
  } else {
    AR_fore = ugarchforecast(AR2_fit, n.ahead = 1)
    dailyreturns = residuals(AR2_fit)
    see_ar[i] <- 2
  }
  VaR[i,length(alpha)*nmodels + 2] = as.numeric(AR_fore@forecast$seriesFor)      # InS mean
  
  # Benchmarks
  GARCH_fit = ugarchfit(GARCH_Spec, dailyreturns, solver = "hybrid")
  GARCH_fore = ugarchforecast(GARCH_fit, n.ahead = 1)
  GJR_fit = ugarchfit(GJR_Spec, dailyreturns, solver = "hybrid")
  GJR_fore = ugarchforecast(GJR_fit, n.ahead = 1)
  
  # Model Estimation
  GAS_fit = UniGASFit(GAS_Spec, dailyreturns, Compute.SE = FALSE)
  GAS_fore = UniGASFor(GAS_fit, H = 1)
  FIGARCH_fit = ugarchfit(FIGARCH_Spec, dailyreturns, solver = "hybrid", fit.control =  list(trunclag = 100))
  FIGARCH_fore = ugarchforecast(FIGARCH_fit, n.ahead = 1)
  NGARCH_fit = ugarchfit(NGARCH_Spec, dailyreturns, solver = "hybrid", solver.control = list(tol = 1e-10))
  NGARCH_fore = ugarchforecast(NGARCH_fit, n.ahead = 1, data = dailyreturns)
  set.seed(i)
  MSGARCH_fit = FitMCMC(MSGARC_Spec,dailyreturns)
  Boot = RobGARCHBoot(dailyreturns, n.boot = 3000, n.ahead = 1, ins = TRUE)
  
  # In-sample volatilities for GARCH, GJR, FIGARCH and NGARCH
  vol_GARCH[,i] = as.numeric(sigma(GARCH_fit))
  vol_GJR[,i] = as.numeric(sigma(GJR_fit))
  vol_FI[,i] = as.numeric(sigma(FIGARCH_fit))
  vol_NG[,i] = as.numeric(sigma(NGARCH_fit))
  
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
  ## NGARCH
  inVaR2_NG[,i] = qdist(distribution = "sstd", alpha[1], mu = 0, sigma = as.vector(sigma(NGARCH_fit)), skew = coef(NGARCH_fit)["skew"], shape = coef(NGARCH_fit)["shape"])
  inVaR5_NG[,i] = qdist(distribution = "sstd", alpha[2], mu = 0, sigma = as.vector(sigma(NGARCH_fit)), skew = coef(NGARCH_fit)["skew"], shape = coef(NGARCH_fit)["shape"])
  VaR_NG = qdist(distribution = "sstd", alpha, mu = 0, sigma = as.numeric(NGARCH_fore@forecast$sigmaFor), skew = coef(NGARCH_fit)["skew"], shape = coef(NGARCH_fit)["shape"])
  ## GARCH
  inVaR2_GARCH[,i] = qdist(distribution = "sstd", alpha[1], mu = 0, sigma = as.vector(sigma(GARCH_fit)), skew = coef(GARCH_fit)["skew"], shape = coef(GARCH_fit)["shape"])
  inVaR5_GARCH[,i] = qdist(distribution = "sstd", alpha[2], mu = 0, sigma = as.vector(sigma(GARCH_fit)), skew = coef(GARCH_fit)["skew"], shape = coef(GARCH_fit)["shape"])
  VaR_GARCH = qdist(distribution = "sstd", alpha, mu = 0, sigma = as.numeric(GARCH_fore@forecast$sigmaFor), skew = coef(GARCH_fit)["skew"], shape = coef(GARCH_fit)["shape"])
  ## GJR
  inVaR2_GJR[,i] = qdist(distribution = "sstd", alpha[1], mu = 0, sigma = as.vector(sigma(GJR_fit)), skew = coef(GJR_fit)["skew"], shape = coef(GJR_fit)["shape"])
  inVaR5_GJR[,i] = qdist(distribution = "sstd", alpha[2], mu = 0, sigma = as.vector(sigma(GJR_fit)), skew = coef(GJR_fit)["skew"], shape = coef(GJR_fit)["shape"])
  VaR_GJR = qdist(distribution = "sstd", alpha, mu = 0, sigma = as.numeric(GJR_fore@forecast$sigmaFor), skew = coef(GJR_fit)["skew"], shape = coef(GJR_fit)["shape"])
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
  
  VaR[i,1:(length(alpha)*nmodels)] = c(VaR_GARCH, VaR_GJR, tail(insampleGAS_VaR,1), risk$VaR, BootVaR, VaR_FI, VaR_NG,
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
  # NGARCH
  f_NG = function(x, sigma_) x*ddist(distribution = "sstd", y = x,
                           mu = 0, sigma = sigma_, skew = coef(NGARCH_fit)["skew"],
                           shape = coef(NGARCH_fit)["shape"])
  ES_NG = c(integrate(f_NG, -Inf, VaR_NG[1], sigma_ = as.numeric(NGARCH_fore@forecast$sigmaFor))$value/0.025,
            integrate(f_NG, -Inf, VaR_NG[2], sigma_ = as.numeric(NGARCH_fore@forecast$sigmaFor))$value/0.050)
  for (j in 1:InS) {
    inES2_NG[j,i] = integrate(f_NG, -Inf, inVaR2_NG[j,i], sigma_ = vol_NG[j,i])$value/0.025
    inES5_NG[j,i] = integrate(f_NG, -Inf, inVaR5_NG[j,i], sigma_ = vol_NG[j,i])$value/0.050
  }
  # GARCH
  f_GARCH = function(x, sigma_) x*ddist(distribution = "sstd", y = x,
                                     mu = 0, sigma = sigma_, skew = coef(GARCH_fit)["skew"],
                                     shape = coef(GARCH_fit)["shape"])
  ES_GARCH = c(integrate(f_GARCH, -Inf, VaR_GARCH[1], sigma_ = as.numeric(GARCH_fore@forecast$sigmaFor))$value/0.025,
            integrate(f_GARCH, -Inf, VaR_GARCH[2], sigma_ = as.numeric(GARCH_fore@forecast$sigmaFor))$value/0.050)
  for (j in 1:InS) {
    inES2_GARCH[j,i] = integrate(f_GARCH, -Inf, inVaR2_GARCH[j,i], sigma_ = vol_GARCH[j,i])$value/0.025
    inES5_GARCH[j,i] = integrate(f_GARCH, -Inf, inVaR5_GARCH[j,i], sigma_ = vol_GARCH[j,i])$value/0.050
  }
  # GJR
  f_GJR = function(x, sigma_) x*ddist(distribution = "sstd", y = x,
                                        mu = 0, sigma = sigma_, skew = coef(GJR_fit)["skew"],
                                        shape = coef(GJR_fit)["shape"])
  ES_GJR = c(integrate(f_GJR, -Inf, VaR_GJR[1], sigma_ = as.numeric(GJR_fore@forecast$sigmaFor))$value/0.025,
               integrate(f_GJR, -Inf, VaR_GJR[2], sigma_ = as.numeric(GJR_fore@forecast$sigmaFor))$value/0.050)
  for (j in 1:InS) {
    inES2_GJR[j,i] = integrate(f_GJR, -Inf, inVaR2_GJR[j,i], sigma_ = vol_GJR[j,i])$value/0.025
    inES5_GJR[j,i] = integrate(f_GJR, -Inf, inVaR5_GJR[j,i], sigma_ = vol_GJR[j,i])$value/0.050
  }
  ## Bootstrap
  BootES = c(mean(Boot[[1]][Boot[[1]] <= BootVaR[1]]),
             mean(Boot[[1]][Boot[[1]] <= BootVaR[2]]))
  for (j in 1:InS) {
    inES2_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,] <= inVaR2_Boot[j,i]])
    inES5_Boot[j,i] = mean(Boot[[3]][j,][Boot[[3]][j,] <= inVaR5_Boot[j,i]])
  }
  # GAS
  insampleGAS_ES = ES(GAS_fit,probs = alpha)
  inES2_GAS[,i] = insampleGAS_ES[1:InS,1]
  inES5_GAS[,i] = insampleGAS_ES[1:InS,2]
  # MSGARCH
  inES2_MS[,i] = insampleRisk$ES[,1]
  inES5_MS[,i] = insampleRisk$ES[,2]
  ## Quantile methods
  inES2_CAViaR[,i] = caviar2[[1]][1:InS,2]
  inES5_CAViaR[,i] = caviar5[[1]][1:InS,2]
  inES2_CAViaREVT[,i] = caviar_evt2[[1]][1:InS,2]
  inES5_CAViaREVT[,i] = caviar_evt5[[1]][1:InS,2]
  inES2_CAViaRALD[,i] = caviar_ald2[[1]][1:InS,2]
  inES5_CAViaRALD[,i] = caviar_ald5[[1]][1:InS,2]
  

  ES[i,] = c(ES_GARCH, ES_GJR, tail(insampleGAS_ES,1), risk$ES, BootES, ES_FI, ES_NG,
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
write.csv(see_ar, paste0(crytocurrency,"see_ar.csv"))

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

# Saving InS VaR and ES NGARCH
write.csv(inVaR2_NG, paste0(crytocurrency,"inVaR2_NG.csv"))
write.csv(inVaR5_NG, paste0(crytocurrency,"inVaR5_NG.csv"))
write.csv(inES2_NG, paste0(crytocurrency,"inES2_NG.csv"))
write.csv(inES5_NG, paste0(crytocurrency,"inES5_NG.csv"))

# Saving InS VaR and ES GARCH
write.csv(inVaR2_GARCH, paste0(crytocurrency,"inVaR2_GARCH.csv"))
write.csv(inVaR5_GARCH, paste0(crytocurrency,"inVaR5_GARCH.csv"))
write.csv(inES2_GARCH, paste0(crytocurrency,"inES2_GARCH.csv"))
write.csv(inES5_GARCH, paste0(crytocurrency,"inES5_GARCH.csv"))

# Saving InS VaR and ES GJR
write.csv(inVaR2_GJR, paste0(crytocurrency,"inVaR2_GJR.csv"))
write.csv(inVaR5_GJR, paste0(crytocurrency,"inVaR5_GJR.csv"))
write.csv(inES2_GJR, paste0(crytocurrency,"inES2_GJR.csv"))
write.csv(inES5_GJR, paste0(crytocurrency,"inES5_GJR.csv"))

# Saving InS VaR and ES Quantile methods
write.csv(inVaR2_CAViaR, paste0(crytocurrency,"inVaR2_CAViaR.csv"))
write.csv(inVaR5_CAViaR, paste0(crytocurrency,"inVaR5_CAViaR.csv"))
write.csv(inES2_CAViaR, paste0(crytocurrency,"inES2_CAViaR.csv"))
write.csv(inES5_CAViaR, paste0(crytocurrency,"inES5_CAViaR.csv"))

write.csv(inVaR2_CAViaREVT, paste0(crytocurrency,"inVaR2_CAViaREVT.csv"))
write.csv(inVaR5_CAViaREVT, paste0(crytocurrency,"inVaR5_CAViaREVT.csv"))
write.csv(inES2_CAViaREVT, paste0(crytocurrency,"inES2_CAViaREVT.csv"))
write.csv(inES5_CAViaREVT, paste0(crytocurrency,"inES5_CAViaREVT.csv"))

write.csv(inVaR2_CAViaRALD, paste0(crytocurrency,"inVaR2_CAViaRALD.csv"))
write.csv(inVaR5_CAViaRALD, paste0(crytocurrency,"inVaR5_CAViaRALD.csv"))
write.csv(inES2_CAViaRALD, paste0(crytocurrency,"inES2_CAViaRALD.csv"))
write.csv(inES5_CAViaRALD, paste0(crytocurrency,"inES5_CAViaRALD.csv"))