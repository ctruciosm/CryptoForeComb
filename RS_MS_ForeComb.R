################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### Relative and Minimum score combining strategies (Taylor, 2020)
################################################################################## 

setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/CryptoForeComb/")
library(Rsolnp)
library(dplyr)
library(stringr)  
library(Rcpp)
sourceCpp("scoring_functions.cpp")
source("Optimizations.R")

cryptocurrency =  "/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/BTC" # "./ETH"  # Options are "./BTC", "./ETH" and "./LTC"
end_date = "2021-08-14"

if (str_sub(cryptocurrency , -3, -1)   == "BTC") {
  crypto = read.csv("./Data/BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% 
    mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

if (str_sub(cryptocurrency, -3, -1)   == "ETH") {
  crypto = read.csv("./Data/ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% 
    mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

setwd(cryptocurrency)
# Load OoS Data
VaR = read.csv("VaR.csv")
ES = read.csv("ES.csv")
mu = VaR[,"mu"]
ret = VaR[,"OoS"]

VaR2 = VaR %>% dplyr::select(ends_with("2")) %>% as.matrix() + mu
VaR5 = VaR %>% dplyr::select(ends_with("5")) %>% as.matrix() + mu

ES2 = ES %>% dplyr::select(ends_with("2")) %>% as.matrix() + mu
ES5 = ES %>% dplyr::select(ends_with("5")) %>% as.matrix() + mu

# Load InS Data

inVaR2_Boot = read.csv("inVaR2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_Boot = read.csv("inVaR5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_Boot = read.csv("inES2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_Boot = read.csv("inES5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_GAS = read.csv("inVaR2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_GAS = read.csv("inVaR5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_GAS = read.csv("inES2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_GAS = read.csv("inES5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_MS = read.csv("inVaR2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_MS = read.csv("inVaR5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_MS = read.csv("inES2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_MS = read.csv("inES5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_FI = read.csv("inVaR2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_FI = read.csv("inVaR5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_FI = read.csv("inES2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_FI = read.csv("inES5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_CS = read.csv("inVaR2_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_CS = read.csv("inVaR5_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_CS = read.csv("inES2_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_CS = read.csv("inES5_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_CAViaR = read.csv("inVaR2_CAViaR.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_CAViaR = read.csv("inVaR5_CAViaR.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_CAViaR = read.csv("inES2_CAViaR.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_CAViaR = read.csv("inES5_CAViaR.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_CAViaREVT = read.csv("inVaR2_CAViaREVT.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_CAViaREVT = read.csv("inVaR5_CAViaREVT.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_CAViaREVT = read.csv("inES2_CAViaREVT.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_CAViaREVT = read.csv("inES5_CAViaREVT.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR2_CAViaRALD = read.csv("inVaR2_CAViaRALD.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_CAViaRALD = read.csv("inVaR5_CAViaRALD.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_CAViaRALD = read.csv("inES2_CAViaRALD.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_CAViaRALD = read.csv("inES5_CAViaRALD.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


# Setup
a2 = 0.025
a5 = 0.050
M = dim(VaR5)[2]
VaR_Comb = VaR_RSC = VaR_AVG = VaR_MSC = matrix(0,ncol = 2,nrow = length(ret))
ES_Comb = ES_RSC = ES_AVG = ES_MSC = matrix(0,ncol = 2,nrow = length(ret))

OoS = length(ret)
InS = dim(crypto)[1] - OoS

colnames(VaR_Comb) = c("2_5%", "5%")
colnames(ES_Comb) = c("2_5%", "5%")

# Procedure

omega2RSC = omega5RSC = omega10RSC = matrix(0,ncol = M,nrow = length(ret))
omega2MSC = omega5MSC = omega10MSC = matrix(0,ncol = 2*M,nrow = length(ret))


for (j in 1:3) {
  if (j == 1) S_ = FZG
  if (j == 2) S_ = NZ
  if (j == 3) S_ = AL

  # Initial values RSC
  lini2 = 0.000001
  lini5 = 0.000001
  # Initial values MSC
  omegaini2 = rep(rep(1/M,M),2)
  omegaini5 = rep(rep(1/M,M),2)  

    for (i in 1:OoS) {
      #### RSC  
      lambda2 = RSC_opt(parini = lini2, 
                        VaR = cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i],inVaR2_FI[,i],
                                   inVaR2_CS[,i], inVaR2_CAViaR[,i], inVaR2_CAViaREVT[,i], inVaR2_CAViaRALD[,i]), 
                        ES = cbind(inES2_Boot[,i], inES2_GAS[,i], inES2_MS[,i], inES2_FI[,i],
                                     inES2_CS[,i], inES2_CAViaR[,i], inES2_CAViaREVT[,i], inES2_CAViaRALD[,i]), 
                        r =  crypto$ret[i:(InS + i - 1)], alpha = a2, S = S_)
      lini2 = lambda2
      lambda5 = RSC_opt(parini = lini5, 
                        VaR = cbind(inVaR5_Boot[,i], inVaR5_GAS[,i], inVaR5_MS[,i],inVaR5_FI[,i],
                                    inVaR5_CS[,i], inVaR5_CAViaR[,i],inVaR5_CAViaREVT[,i],inVaR5_CAViaRALD[,i]), 
                        ES = cbind(inES5_Boot[,i], inES5_GAS[,i], inES5_MS[,i], inES5_FI[,i],
                                   inES5_CS[,i], inES5_CAViaR[,i], inES5_CAViaREVT[,i], inES5_CAViaRALD[,i]), 
                        r =  crypto$ret[i:(InS + i - 1)], alpha = a5, S = S_)
      lini5 = lambda5
      
      omega2RSC[i,] = RSC_Eval(lambda2, 
                               VaR = cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i], inVaR2_FI[,i],
                                           inVaR2_CS[,i], inVaR2_CAViaR[,i], inVaR2_CAViaREVT[,i], inVaR2_CAViaRALD[,i]), 
                               ES = cbind(inES2_Boot[,i], inES2_GAS[,i], inES2_MS[,i], inES2_FI[,i],
                                          inES2_CS[,i], inES2_CAViaR[,i], inES2_CAViaREVT[,i], inES2_CAViaRALD[,i]), 
                               r =  crypto$ret[i:(InS + i - 1)], alpha = a2, S = S_)
      omega5RSC[i,] = RSC_Eval(lambda5, 
                               VaR = cbind(inVaR5_Boot[,i],inVaR5_GAS[,i],inVaR5_MS[,i],inVaR5_FI[,i],
                                           inVaR5_CS[,i],inVaR5_CAViaR[,i],inVaR5_CAViaREVT[,i], inVaR5_CAViaRALD[,i]), 
                               ES = cbind(inES5_Boot[,i],inES5_GAS[,i],inES5_MS[,i],inES5_FI[,i],
                                          inES5_CS[,i],inES5_CAViaR[,i],inES5_CAViaREVT[,i],inES5_CAViaRALD[,i]), 
                               r =  crypto$ret[i:(InS + i - 1)], alpha = a5, S = S_)

      
      VaR_RSC[i,] = c(VaR2[i,] %*% omega2RSC[i,],VaR5[i,] %*% omega5RSC[i,])
      ES_RSC[i,]  = c(ES2[i,] %*% omega2RSC[i,], ES5[i,] %*% omega5RSC[i,])   
      
      #### MSC
      omega2MSC[i,] = MSC_opt(parini = omegaini2, 
                              VaR = cbind(inVaR5_Boot[,i], inVaR5_GAS[,i], inVaR5_MS[,i],inVaR5_FI[,i],
                                          inVaR5_CS[,i], inVaR5_CAViaR[,i],inVaR5_CAViaREVT[,i],inVaR5_CAViaRALD[,i]), 
                              ES = cbind(inES5_Boot[,i], inES5_GAS[,i], inES5_MS[,i], inES5_FI[,i],
                                         inES5_CS[,i], inES5_CAViaR[,i], inES5_CAViaREVT[,i], inES5_CAViaRALD[,i]), 
                              r =  crypto$ret[i:(InS + i - 1)], alpha = a5, S = S_)
      omega5MSC[i,] = MSC_opt(parini = omegaini5, 
                              VaR = cbind(inVaR5_Boot[,i], inVaR5_GAS[,i], inVaR5_MS[,i],inVaR5_FI[,i],
                                          inVaR5_CS[,i], inVaR5_CAViaR[,i],inVaR5_CAViaREVT[,i],inVaR5_CAViaRALD[,i]), 
                              ES = cbind(inES5_Boot[,i], inES5_GAS[,i], inES5_MS[,i], inES5_FI[,i],
                                         inES5_CS[,i], inES5_CAViaR[,i], inES5_CAViaREVT[,i], inES5_CAViaRALD[,i]), 
                              r =  crypto$ret[i:(InS + i - 1)], alpha = a5, S = S_)
      
      omegaini2 = omega2MSC[i,]
      omegaini5 = omega5MSC[i,]
      VaR_MSC[i,] = c(VaR2[i,] %*% omega2MSC[i,1:M],VaR5[i,] %*% omega5MSC[i,1:M])
      ES_MSC[i,] = c(ES2[i,] %*% omega2MSC[i,(M + 1):(2*M)],ES5[i,] %*% omega5MSC[i,(M+1):(2*M)])  
    }
  
  # Once we compute the combining estimators, we save the results
  if (j == 1) {
    write.table(VaR_RSC,"VaR_RSC_FZG.csv")
    write.table(ES_RSC,"ES_RSC_FZG.csv")
    
    write.table(VaR_MSC,"VaR_MSC_FZG.csv")
    write.table(ES_MSC,"ES_MSC_FZG.csv")
  }
  if (j == 2) {
    write.table(VaR_RSC,"VaR_RSC_NZ.csv")
    write.table(ES_RSC,"ES_RSC_NZ.csv")
    
    write.table(VaR_MSC,"VaR_MSC_NZ.csv")
    write.table(ES_MSC,"ES_MSC_NZ.csv")
  }
  if (j == 3) {
    write.table(VaR_RSC,"VaR_RSC_AL.csv")
    write.table(ES_RSC,"ES_RSC_AL.csv")
    
    write.table(VaR_MSC,"VaR_MSC_AL.csv")
    write.table(ES_MSC,"ES_MSC_AL.csv")
  }
}


  
