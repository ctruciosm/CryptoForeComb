####################################################
###     VaR estimation for GARCH-Type models     ###
### Only for readers interested in those results ###
####################################################

library(rugarch)
library(dplyr)

crytocurrency = "BTC/" # Other options are: "ETH/", "LTC/", and "XRP/"


if(crytocurrency == "BTC/"){
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
  setwd("./Data/BTC")
}

if(crytocurrency == "ETH/"){
  setwd("./ETH")
  crypto = read.csv("ETHUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
}

if(crytocurrency == "LTC/"){
  setwd("./LTC")
  crypto = read.csv("LTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < "2020-09-18")
}

if(crytocurrency == "XRP/"){
  setwd("./XRP")
  crypto =  read.csv("XRPUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2018-05-04", date < "2020-09-18") 
}


OoS = 365
InS = dim(crypto)[1]-OoS

VaR = matrix(0,ncol= 5, nrow = OoS)
ES = matrix(0,ncol= 3, nrow = OoS)
inVaR1 = inVaR2 = inVaR5 =  matrix(0, ncol = OoS, nrow = InS)
distri =  c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp","jsu")

# Modelos GARCH, IGARCH, EGARCH, GJR, csGARCH, AVGARCH

# sGARCH
s = "sGARCH"
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = s, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    tryCatch({
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      e = as.numeric(dailyreturns/sigma(GARCH_fit))
      param = fitdist(distribution = d, e)$par
      VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                         mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                         sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                         skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                         shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
      
      
      f = function(x) qdist(distribution = d, p = x, 
                            mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                            sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                            skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                            shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
      
      
      ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
      
    }, error = function(e) {
      VaR[i,1:3] = c(NA,NA,NA)
      ES[i,1:3] = c(NA,NA,NA)
    })
    
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  write.csv(ES, paste0(paste0(s,d),"ES.csv"))
}

# IGARCH
s = "iGARCH"
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = s, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    tryCatch({
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      e = as.numeric(dailyreturns/sigma(GARCH_fit))
      param = fitdist(distribution = d, e)$par
      VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                         mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                         sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                         skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                         shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
      
      
      f = function(x) qdist(distribution = d, p = x, 
                            mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                            sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                            skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                            shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
      
      
      ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
      
    }, error = function(e) {
      VaR[i,1:3] = c(NA,NA,NA)
      ES[i,1:3] = c(NA,NA,NA)
    })
    
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  write.csv(ES, paste0(paste0(s,d),"ES.csv"))
}

# GJR
s = "gjrGARCH"
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = s, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    tryCatch({
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      e = as.numeric(dailyreturns/sigma(GARCH_fit))
      param = fitdist(distribution = d, e)$par
      VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                         mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                         sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                         skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                         shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
      
      
      f = function(x) qdist(distribution = d, p = x, 
                            mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                            sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                            skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                            shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
      
      
      ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
      
    }, error = function(e) {
      VaR[i,1:3] = c(NA,NA,NA)
      ES[i,1:3] = c(NA,NA,NA)
    })
    
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  write.csv(ES, paste0(paste0(s,d),"ES.csv"))
}

# csGARCH
s = "csGARCH"
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = s, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    tryCatch({
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      e = as.numeric(dailyreturns/sigma(GARCH_fit))
      param = fitdist(distribution = d, e)$par
      VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                         mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                         sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                         skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                         shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
      
      
      f = function(x) qdist(distribution = d, p = x, 
                            mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                            sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                            skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                            shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
      
      
      ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
      
    }, error = function(e) {
      VaR[i,1:3] = c(NA,NA,NA)
      ES[i,1:3] = c(NA,NA,NA)
    })
    
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  write.csv(ES, paste0(paste0(s,d),"ES.csv"))
}

# AVGARCH
s = "AVGARCH"
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d, fixed.pars = list(gamma1 = 0, delta = 1))
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    tryCatch({
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      e = as.numeric(dailyreturns/sigma(GARCH_fit))
      param = fitdist(distribution = d, e)$par
      VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                         mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                         sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                         skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                         shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
      
      
      f = function(x) qdist(distribution = d, p = x, 
                            mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                            sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                            skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                            shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
      
      
      ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                    as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
      
    }, error = function(e) {
      VaR[i,1:3] = c(NA,NA,NA)
      ES[i,1:3] = c(NA,NA,NA)
    })
    
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  write.csv(ES, paste0(paste0(s,d),"ES.csv"))
}



# FGARCH - submodels: 
submodels = c("TGARCH", "NGARCH", "NAGARCH", "APARCH")
for (s in submodels){
  for(d in distri){
    GARCH_spec = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1),  submodel = s), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
    for (i in 1:OoS){
      predailyreturns = crypto$ret[i:(InS+i-1)]
      VaR[i,4] = mean(predailyreturns)
      VaR[i,5] = crypto$ret[InS+i]
      dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
      
      tryCatch({
        GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid",solver.control = list(tol = 1e-10))
        GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
        
        e = as.numeric(dailyreturns/sigma(GARCH_fit))
        param = fitdist(distribution = d, e)$par
        VaR[i,1:3] = qdist(distribution = d, c(0.01,0.025,0.05), 
                           mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                           sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                           skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                           shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))*as.numeric(GARCH_for@forecast$sigmaFor)
        
        
        f = function(x) qdist(distribution = d, p = x, 
                              mu = ifelse(is.na(param["mu"]), NULL, param["mu"]),
                              sigma = ifelse(is.na(param["sigma"]), NULL, param["sigma"]),
                              skew = ifelse(is.na(param["skew"]), NULL, param["skew"]),
                              shape = ifelse(is.na(param["shape"]), NULL, param["shape"]))
        
        
        ES[i,1:3] = c(as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.010)$value/0.010, 
                      as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.025)$value/0.025, 
                      as.numeric(GARCH_for@forecast$sigmaFor)*integrate(f, 0, 0.050)$value/0.050) 
        
      }, error = function(e) {
        VaR[i,1:3] = c(NA,NA,NA)
        ES[i,1:3] = c(NA,NA,NA)
      })
      
      print(paste(i,"de", OoS, "replicacoes", s, d))
    } 
    write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
    write.csv(ES, paste0(paste0(s,d),"ES.csv"))
  }
}





