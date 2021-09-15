################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### Relative and Minimum score combining strategies (Taylor, 2020)
################################################################################## 

library(Rsolnp)
library(dplyr)
library(stringr)  
library(Rcpp)
sourceCpp("scoring_functions.cpp")
source("Optimizations.R")

cryptocurrency = "./ETH"  # Pptions are "./BTC", "./ETH" and "./LTC"
end_date = "2021-08-14"

if(str_sub(cryptocurrency , - 3, - 1)   == "BTC"){
  crypto = read.csv("./Data/BTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% 
    mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

if(str_sub(cryptocurrency, - 3, - 1)   == "ETH"){
  crypto = read.csv("./Data/ETHUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% 
    mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date)
}

if(str_sub(cryptocurrency, - 3, - 1)  == "LTC"){
  crypto = read.csv("./Data/LTCUSDT-1d-data.csv") %>% 
    mutate(date = as.Date(timestamp)) %>% arrange(date) %>% 
    mutate(ret = c(0,diff(log(close))*100)) %>% 
    dplyr::select(date, ret) %>% filter(date > "2017-12-13", date < end_date)
}

setwd(cryptocurrency)
# Load OoS Data
VaR = read.csv("VaR.csv")
ES = read.csv("ES.csv")
mu = VaR[,"mu"]
ret = VaR[,"OoS"]

VaR2 = VaR %>% dplyr::select(ends_with("2")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu
VaR5 = VaR %>% dplyr::select(ends_with("5")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu
VaR10 = VaR %>% dplyr::select(ends_with("10")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu

ES2 = ES %>% dplyr::select(ends_with("2")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu
ES5 = ES %>% dplyr::select(ends_with("5")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu
ES10 = ES %>% dplyr::select(ends_with("10")) %>% dplyr::select(-contains("CSGARCH"), -contains("FIGARCH")) %>% as.matrix() + mu

# Load InS Data

inVaR2_Boot = read.csv("inVaR2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_Boot = read.csv("inVaR5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_Boot = read.csv("inVaR10_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_Boot = read.csv("inES2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_Boot = read.csv("inES5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_Boot = read.csv("inES10_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


inVaR2_GAS = read.csv("inVaR2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_GAS = read.csv("inVaR5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_GAS = read.csv("inVaR10_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_GAS = read.csv("inES2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_GAS = read.csv("inES5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_GAS = read.csv("inES10_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


inVaR2_MS = read.csv("inVaR2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_MS = read.csv("inVaR5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR10_MS = read.csv("inVaR10_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_MS = read.csv("inES2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_MS = read.csv("inES5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES10_MS = read.csv("inES10_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

#inVaR2_FI = read.csv("inVaR2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inVaR5_FI = read.csv("inVaR5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inVaR10_FI = read.csv("inVaR10_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES2_FI = read.csv("inES2_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES5_FI = read.csv("inES5_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES10_FI = read.csv("inES10_FI.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

#inVaR2_CS = read.csv("inVaR2_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inVaR5_CS = read.csv("inVaR5_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inVaR10_CS = read.csv("inVaR10_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES2_CS = read.csv("inES2_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES5_CS = read.csv("inES5_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
#inES10_CS = read.csv("inES10_CS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)


# Setup
a2 = 0.025
a5 = 0.050
a10 = 0.10
M = dim(VaR5)[2]
VaR_Comb = VaR_RSC = VaR_AVG = VaR_MSC = matrix(0,ncol = 3,nrow = length(ret))
ES_Comb = ES_RSC = ES_AVG = ES_MSC = matrix(0,ncol = 3,nrow = length(ret))

OoS = length(ret)
InS = dim(crypto)[1]-OoS

colnames(VaR_Comb) = c("2_5%", "5%", "10%")
colnames(ES_Comb) = c("2_5%", "5%", "10%")

# Procedure

omega2RSC = omega5RSC = omega10RSC = matrix(0,ncol=M,nrow = length(ret))
omega2MSC = omega5MSC = omega10MSC = matrix(0,ncol=2*M,nrow = length(ret))


for (j in 1:3){
  if (j == 1) S_ = FZG
  if (j == 2) S_ = NZ
  if (j == 3) S_ = AL

  # Initial values RSC
  lini2 = 0.000001
  lini5 = 0.000001
  lini10= 0.000001
  # Initial values MSC
  omegaini2 = rep(rep(1/M,M),2)
  omegaini5 = rep(rep(1/M,M),2)  
  omegaini10= rep(rep(1/M,M),2)  
  
    for (i in 1:OoS){
      #### RSC  
      tryCatch({
        lambda2 = RSC_opt(parini = lini2, 
                        VaR= cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), 
                        ES = cbind(inES2_Boot[,i], inES2_GAS[,i],  inES2_MS[,i]), 
                        r =  crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      }, error = function(e) {
        lambda2 = RSC_opt(parini = 0.000001, 
                          VaR= cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), 
                          ES = cbind(inES2_Boot[,i], inES2_GAS[,i],  inES2_MS[,i]), 
                          r =  crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      })
      lini2 = lambda2
      tryCatch({
        lambda5 = RSC_opt(parini = lini5, 
                        VaR= cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), 
                        ES = cbind(inES5_Boot[,i], inES5_GAS[,i],  inES5_MS[,i]), 
                        r =  crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      }, error = function(e) {
        lambda5 = RSC_opt(parini = 0.000001, 
                          VaR= cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), 
                          ES = cbind(inES5_Boot[,i], inES5_GAS[,i],  inES5_MS[,i]), 
                          r =  crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      })
      lini5 = lambda5
      tryCatch({
        lambda10 = RSC_opt(parini = lini10, 
                        VaR= cbind(inVaR10_Boot[,i],inVaR10_GAS[,i], inVaR10_MS[,i]), 
                        ES = cbind(inES10_Boot[,i], inES10_GAS[,i],  inES10_MS[,i]), 
                        r =  crypto$ret[i:(InS+i-1)], alpha = a10, S = S_)
      }, error = function(e) {
        lambda10 = RSC_opt(parini = 0.000001, 
                           VaR= cbind(inVaR10_Boot[,i],inVaR10_GAS[,i], inVaR10_MS[,i]), 
                           ES = cbind(inES10_Boot[,i], inES10_GAS[,i],  inES10_MS[,i]), 
                           r =  crypto$ret[i:(InS+i-1)], alpha = a10, S = S_)
      })
      lini10 = lambda10
      omega2RSC[i,] = RSC_Eval(lambda2, 
                               VaR= cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), 
                               ES = cbind(inES2_Boot[,i], inES2_GAS[,i],  inES2_MS[,i]), 
                               r = crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      omega5RSC[i,] = RSC_Eval(lambda5, 
                               VaR= cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), 
                               ES = cbind(inES5_Boot[,i], inES5_GAS[,i],  inES5_MS[,i]), 
                               r = crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      omega10RSC[i,]= RSC_Eval(lambda10, 
                               VaR= cbind(inVaR10_Boot[,i],inVaR10_GAS[,i], inVaR10_MS[,i]), 
                               ES = cbind(inES10_Boot[,i], inES10_GAS[,i],  inES10_MS[,i]), 
                               r = crypto$ret[i:(InS+i-1)], alpha = a10, S = S_)
      
      VaR_RSC[i,]= c(VaR2[i,]%*%omega2RSC[i,],VaR5[i,]%*%omega5RSC[i,],VaR10[i,]%*%omega10RSC[i,])
      ES_RSC[i,] = c(ES2[i,]%*%omega2RSC[i,], ES5[i,]%*%omega5RSC[i,], ES10[i,]%*%omega10RSC[i,])   
      
      #### MSC
      tryCatch({
        omega2MSC[i,] = MSC_opt(parini = omegaini2, 
                              VaR= cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), 
                              ES = cbind(inES2_Boot[,i], inES2_GAS[,i],  inES2_MS[,i]), 
                              r = crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      }, error = function(e) {
        omega2MSC[i,] = MSC_opt(parini = rep(rep(1/M,M),2), 
                                VaR= cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), 
                                ES = cbind(inES2_Boot[,i], inES2_GAS[,i],  inES2_MS[,i]), 
                                r = crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      })
      tryCatch({
        omega5MSC[i,] = MSC_opt(parini = omegaini5, 
                              VaR= cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), 
                              ES = cbind(inES5_Boot[,i], inES5_GAS[,i],  inES5_MS[,i]), 
                              r = crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      }, error = function(e) {
        omega5MSC[i,] = MSC_opt(parini = rep(rep(1/M,M),2), 
                                VaR= cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), 
                                ES = cbind(inES5_Boot[,i], inES5_GAS[,i],  inES5_MS[,i]), 
                                r = crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      })  
      tryCatch({
        omega10MSC[i,]= MSC_opt(parini = omegaini10, 
                              VaR= cbind(inVaR10_Boot[,i],inVaR10_GAS[,i], inVaR10_MS[,i]), 
                              ES = cbind(inES10_Boot[,i], inES10_GAS[,i],  inES10_MS[,i]), 
                              r = crypto$ret[i:(InS+i-1)], alpha = a10, S = S_)
      }, error = function(e) {
        omega10MSC[i,]= MSC_opt(parini = rep(rep(1/M,M),2), 
                                VaR= cbind(inVaR10_Boot[,i],inVaR10_GAS[,i], inVaR10_MS[,i]), 
                                ES = cbind(inES10_Boot[,i], inES10_GAS[,i],  inES10_MS[,i]), 
                                r = crypto$ret[i:(InS+i-1)], alpha = a10, S = S_)
      })   
      
      omegaini2 = omega2MSC[i,]
      omegaini5 = omega5MSC[i,]
      omegaini10= omega10MSC[i,]
      VaR_MSC[i,] = c(VaR2[i,]%*%omega2MSC[i,1:M],VaR5[i,]%*%omega5MSC[i,1:M],VaR10[i,]%*%omega10MSC[i,1:M])
      ES_MSC[i,] = c(ES2[i,]%*%omega2MSC[i,(M+1):(2*M)],ES5[i,]%*%omega5MSC[i,(M+1):(2*M)],ES10[i,]%*%omega10MSC[i,(M+1):(2*M)])  
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


  
