####################################################
###     VaR estimation for GARCH-Type models     ###
### Only for readers interested in those results ###
####################################################

library(rugarch)
library(dplyr)

crytocurrency = "BTC/" # Other options are: "ETH/", "LTC/", and "XRP/"


if(crytocurrency == "BTC/"){
  setwd("./BTC")
  crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-09-18")
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
inVaR1 = inVaR2 = inVaR5 =  matrix(0, ncol = OoS, nrow = InS)
distri =  c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp","jsu")

# Modelos GARCH, IGARCH, EGARCH, GJR, csGARCH, AVGARCH

# sGARCH
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "sGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# IGARCH
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "iGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# EGARCH
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "eGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# GJR
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "gjrGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# csGARCH
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
  for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "csGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# AVGARCH
for(d in distri){
  GARCH_spec = ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d, fixed.pars = list(gamma1 = 0, delta = 1))
   for (i in 1:OoS){
    predailyreturns = crypto$ret[i:(InS+i-1)]
    VaR[i,4] = mean(predailyreturns)
    VaR[i,5] = crypto$ret[InS+i]
    dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
    
    GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
    GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
    
    inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
    inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
    inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
    
    VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
    print(paste(i,"de", OoS, "replicacoes", s, d))
  }
  s = "AVGARCH"
  write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
  write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
  write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
}

# FGARCH - submodels: 
submodels = c("TGARCH", "NGARCH", "NAGARCH", "APARCH", "ALLGARCH")
for (s in submodels){
  for(d in distri){
    GARCH_spec = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1),  submodel = s), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = d)
    for (i in 1:OoS){
      predailyreturns = crypto$ret[i:(InS+i-1)]
      VaR[i,4] = mean(predailyreturns)
      VaR[i,5] = crypto$ret[InS+i]
      dailyreturns = scale(predailyreturns , scale = FALSE, center = TRUE)
      
      GARCH_fit  = ugarchfit(GARCH_spec, dailyreturns, solver = "hybrid")
      GARCH_for  = ugarchforecast(GARCH_fit, n.ahead = 1, data = dailyreturns)
      
      inVaR1[,i] = as.numeric(quantile(GARCH_fit, 0.010))
      inVaR2[,i] = as.numeric(quantile(GARCH_fit, 0.025))
      inVaR5[,i] = as.numeric(quantile(GARCH_fit, 0.050))
      
      VaR[i,1:3] = c(quantile(GARCH_for, 0.01), quantile(GARCH_for, 0.025), quantile(GARCH_for, 0.05))
      print(paste(i,"de", OoS, "replicacoes", s, d))
    }
    write.csv(inVaR1, paste0(paste0(s,d),"inVaR1.csv"))
    write.csv(inVaR2, paste0(paste0(s,d),"inVaR2.csv"))
    write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
    write.csv(inVaR5, paste0(paste0(s,d),"inVaR5.csv"))
    write.csv(VaR, paste0(paste0(s,d),"VaR.csv"))
  }
}





