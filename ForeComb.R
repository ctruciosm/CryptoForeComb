###########################################
### Forecasting Combination VaR and ES  ###
### Combinations                        ###
###########################################
library(psych)
library(Rsolnp)
library(dplyr)
setwd("./Data/BTC/")
#setwd("./Data/ETH/")
#setwd("./Data/LTC/")
#setwd("./Data/XRP/")


# Load OoS Data
mu = read.csv("VaR.csv")[,"mu"]
ret = read.csv("VaR.csv")[,"OoS"]
VaR1 = as.matrix(read.csv("VaR.csv")[,c("GAS1","MSGARCH1","Boot1")]) + mu
ES1 = as.matrix(read.csv("ES.csv")[,c("GAS1","MSGARCH1","Boot1")]) + mu
VaR2 = as.matrix(read.csv("VaR.csv")[,c("GAS2","MSGARCH2","Boot2")]) +mu
ES2 = as.matrix(read.csv("ES.csv")[,c("GAS2","MSGARCH2","Boot2")])+mu
VaR5 = as.matrix(read.csv("VaR.csv")[,c("GAS5","MSGARCH5","Boot5")])+mu
ES5 = as.matrix(read.csv("ES.csv")[,c("GAS5","MSGARCH5","Boot5")])+mu

crypto = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < "2020-08-18")

# Load InS Data

inVaR1_Boot = read.csv("inVaR1_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_Boot = read.csv("inVaR2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_Boot = read.csv("inVaR5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_Boot = read.csv("inES1_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_Boot = read.csv("inES2_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_Boot = read.csv("inES5_Boot.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR1_GAS = read.csv("inVaR1_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_GAS = read.csv("inVaR2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_GAS = read.csv("inVaR5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_GAS = read.csv("inES1_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_GAS = read.csv("inES2_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_GAS = read.csv("inES5_GAS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

inVaR1_MS = read.csv("inVaR1_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR2_MS = read.csv("inVaR2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inVaR5_MS = read.csv("inVaR5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES1_MS = read.csv("inES1_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES2_MS = read.csv("inES2_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)
inES5_MS = read.csv("inES5_MS.csv")[,-1] + matrix(rep(mu,length(ret)), ncol = length(ret), byrow = TRUE)

# Setup
a1 = 0.010
a2 = 0.025
a5 = 0.050
M = dim(VaR1)[2]
VaR_Comb = VaR_RSC = VaR_AVG = VaR_MSC = matrix(0,ncol = M,nrow = length(ret))
ES_Comb = ES_RSC = ES_AVG = ES_MSC = matrix(0,ncol = M,nrow = length(ret))

OoS = length(ret)
InS = dim(crypto)[1]-OoS

colnames(VaR_Comb) = c("1%", "2_5%", "5%")
colnames(ES_Comb) = c("1%", "2_5%", "5%")

# Procedure

omega1RSC = omega2RSC = omega5RSC = matrix(0,ncol=M,nrow = length(ret))
omega1MSC = omega2MSC = omega5MSC = matrix(0,ncol=2*M,nrow = length(ret))


for (j in 1:3){
  if (j == 1) S_ = FZG
  if (j == 2) S_ = NZ
  if (j == 3) S_= AL

    for (i in 1:OoS){
      #### RSC  
      lambda1 = RSC_opt(VaR = cbind(inVaR1_Boot[,i],inVaR1_GAS[,i], inVaR1_MS[,i]), ES = cbind(inES1_Boot[,i],inES1_GAS[,i], inES1_MS[,i]), r =  crypto$ret[i:(InS+i-1)], alpha = a1, S = S_)
      lambda2 = RSC_opt(VaR = cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), ES = cbind(inES2_Boot[,i],inES2_GAS[,i], inES2_MS[,i]), r =  crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      lambda5 = RSC_opt(VaR = cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), ES = cbind(inES5_Boot[,i],inES5_GAS[,i], inES5_MS[,i]), r =  crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      
      omega1RSC[i,] = RSC_Eval(lambda1, VaR = cbind(inVaR1_Boot[,i],inVaR1_GAS[,i], inVaR1_MS[,i]), ES = cbind(inES1_Boot[,i],inES1_GAS[,i], inES1_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a1, S = S_)
      omega2RSC[i,] = RSC_Eval(lambda2, VaR = cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), ES = cbind(inES2_Boot[,i],inES2_GAS[,i], inES2_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      omega5RSC[i,] = RSC_Eval(lambda5, VaR = cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), ES = cbind(inES5_Boot[,i],inES5_GAS[,i], inES5_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      
      VaR_RSC[i,] = c(VaR1[i,]%*%omega1RSC[i,],VaR2[i,]%*%omega2RSC[i,],VaR5[i,]%*%omega5RSC[i,])
      ES_RSC[i,] = c(ES1[i,]%*%omega1RSC[i,],ES2[i,]%*%omega2RSC[i,],ES5[i,]%*%omega5RSC[i,])   
      
      #### Average
      EW = rep(1/M,M)
      VaR_AVG[i,] = c(VaR1[i,]%*%EW,VaR2[i,]%*%EW,VaR5[i,]%*%EW)
      ES_AVG[i,] = c(ES1[i,]%*%EW,ES2[i,]%*%EW,ES5[i,]%*%EW)   
      
      #### MSC
      omega1MSC[i,] = MSC_opt(VaR = cbind(inVaR1_Boot[,i],inVaR1_GAS[,i], inVaR1_MS[,i]), ES = cbind(inES1_Boot[,i],inES1_GAS[,i], inES1_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a1, S = S_)
      omega2MSC[i,] = MSC_opt(VaR = cbind(inVaR2_Boot[,i],inVaR2_GAS[,i], inVaR2_MS[,i]), ES = cbind(inES2_Boot[,i],inES2_GAS[,i], inES2_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a2, S = S_)
      omega5MSC[i,] = MSC_opt(VaR = cbind(inVaR5_Boot[,i],inVaR5_GAS[,i], inVaR5_MS[,i]), ES = cbind(inES5_Boot[,i],inES5_GAS[,i], inES5_MS[,i]), r = crypto$ret[i:(InS+i-1)], alpha = a5, S = S_)
      
      VaR_MSC[i,] = c(VaR1[i,]%*%omega1MSC[i,1:M],VaR2[i,]%*%omega2MSC[i,1:M],VaR5[i,]%*%omega5MSC[i,1:M])
      ES_MSC[i,] = c(ES1[i,]%*%omega1MSC[i,(M+1):(2*M)],ES2[i,]%*%omega2MSC[i,(M+1):(2*M)],ES5[i,]%*%omega5MSC[i,(M+1):(2*M)])  
      
    }
  
  
  if (j == 1) {
    write.table(omega1RSC,"omega1_RSC_FZG.csv")
    write.table(omega2RSC,"omega2_RSC_FZG.csv")
    write.table(omega1RSC,"omega5_RSC_FZG.csv")
    
    write.table(VaR_RSC,"VaR_RSC_FZG.csv")
    write.table(ES_RSC,"ES_RSC_FZG.csv")
    
    write.table(VaR_AVG,"VaR_AVG_FZG.csv")
    write.table(ES_AVG,"ES_AVG_FZG.csv")
    
    write.table(omega1MSC,"omega1_MSC_FZG.csv")
    write.table(omega2MSC,"omega2_MSC_FZG.csv")
    write.table(omega1MSC,"omega5_MSC_FZG.csv")
    
    write.table(VaR_MSC,"VaR_MSC_FZG.csv")
    write.table(ES_MSC,"ES_MSC_FZG.csv")
  }
  
  if (j == 2) {
    write.table(omega1RSC,"omega1_RSC_NZ.csv")
    write.table(omega2RSC,"omega2_RSC_NZ.csv")
    write.table(omega1RSC,"omega5_RSC_NZ.csv")
    
    write.table(VaR_RSC,"VaR_RSC_NZ.csv")
    write.table(ES_RSC,"ES_RSC_NZ.csv")
    
    write.table(VaR_AVG,"VaR_AVG_NZ.csv")
    write.table(ES_AVG,"ES_AVG_NZ.csv")
    
    write.table(omega1MSC,"omega1_MSC_NZ.csv")
    write.table(omega2MSC,"omega2_MSC_NZ.csv")
    write.table(omega1MSC,"omega5_MSC_NZ.csv")
    
    write.table(VaR_MSC,"VaR_MSC_NZ.csv")
    write.table(ES_MSC,"ES_MSC_NZ.csv")
  }
  
  if (j == 3) {
    write.table(omega1RSC,"omega1_RSC_AL.csv")
    write.table(omega2RSC,"omega2_RSC_AL.csv")
    write.table(omega1RSC,"omega5_RSC_AL.csv")
    
    write.table(VaR_RSC,"VaR_RSC_AL.csv")
    write.table(ES_RSC,"ES_RSC_AL.csv")
    
    write.table(VaR_AVG,"VaR_AVG_AL.csv")
    write.table(ES_AVG,"ES_AVG_AL.csv")
    
    write.table(omega1MSC,"omega1_MSC_AL.csv")
    write.table(omega2MSC,"omega2_MSC_AL.csv")
    write.table(omega1MSC,"omega5_MSC_AL.csv")
    
    write.table(VaR_MSC,"VaR_MSC_AL.csv")
    write.table(ES_MSC,"ES_MSC_AL.csv")
  }


}


setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/CryptoForeComb/")

  
