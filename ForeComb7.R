###########################################
### Forecasting Combination VaR and ES  ###
### Combinations                        ###
###########################################
library(psych)
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/BTC/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/ETH/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/LTC/")
#setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/XRP/")

# Load Data 
mu = read.csv("VaR.csv")[,"mu"]
ret = read.csv("VaR.csv")[,"OoS"]

VaR1 = as.matrix(read.csv("VaR.csv")[,c("GAS1","MSGARCH1","Boot1")]) + mu
ES1 = as.matrix(read.csv("ES.csv")[,c("GAS1","MSGARCH1","Boot1")]) + mu

VaR2 = as.matrix(read.csv("VaR.csv")[,c("GAS2","MSGARCH2","Boot2")]) +mu
ES2 = as.matrix(read.csv("ES.csv")[,c("GAS2","MSGARCH2","Boot2")])+mu

VaR5 = as.matrix(read.csv("VaR.csv")[,c("GAS5","MSGARCH5","Boot5")])+mu
ES5 = as.matrix(read.csv("ES.csv")[,c("GAS5","MSGARCH5","Boot5")])+mu


a1 = 0.010
a2 = 0.025
a5 = 0.050
M = dim(VaR1)[2]
VaR_Comb = matrix(0,ncol = M,nrow = length(ret))
ES_Comb =  matrix(0,ncol = M,nrow = length(ret))

colnames(VaR_Comb) = c("1%", "2_5%", "5%")
colnames(ES_Comb) = c("1%", "2_5%", "5%")

# Procedure

omega1 = omega2 = omega5 = matrix(0,ncol=M,nrow = length(ret))

for (j in 1:3){
  if (j == 1) S_ = AL
  if (j == 2) S_ = NZ
  if (j == 3) S_= FZG

    for (i in 8:length(ret)){
    ini1 = RSC_grid(VaR = VaR1[(i-7):(i-1),], ES = ES1[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a1, S = S_)
    lambda1 = suppressWarnings(optim(par = ini1[2], fn = RSC, method = "L-BFGS-B", VaR = VaR1[(i-7):(i-1),], ES = ES1[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a1, S = S_, lower = 0, upper = ini1[1])$par)
    
    ini2 = RSC_grid(VaR = VaR2[(i-7):(i-1),], ES = ES2[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a2, S = S_)
    lambda2 = suppressWarnings(optim(par = ini2[2], fn = RSC, method = "L-BFGS-B", VaR = VaR2[(i-7):(i-1),], ES = ES2[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a2, S = S_, lower = 0, upper = ini2[1])$par)
    #lambda2 = suppressWarnings(nlminb(start = ini2[2], objective = RSC, VaR = VaR2[(i-7):(i-1),], ES = ES2[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a2, S = S_, lower = 0, upper = ini2[1])$par)
    
    ini5 = RSC_grid(VaR = VaR5[(i-7):(i-1),], ES = ES5[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a5, S = S_)
    lambda5 = suppressWarnings(optim(par = ini5[2], fn = RSC, method = "L-BFGS-B", VaR = VaR5[(i-7):(i-1),], ES = ES5[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a5, S = S_, lower = 0, upper = ini5[1])$par)

    omega1[i,] = RSC_Eval(lambda1, VaR = VaR1[(i-7):(i-1),], ES = ES1[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a1, S = S_)
    omega2[i,] = RSC_Eval(lambda2, VaR = VaR2[(i-7):(i-1),], ES = ES2[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a2, S = S_)
    omega5[i,] = RSC_Eval(lambda5, VaR = VaR5[(i-7):(i-1),], ES = ES5[(i-7):(i-1),], r = ret[(i-7):(i-1)], alpha = a5, S = S_)
  
    VaR_Comb[i,] = c(VaR1[i,]%*%omega1[i,],VaR2[i,]%*%omega2[i,],VaR5[i,]%*%omega5[i,])
    ES_Comb[i,] = c(ES1[i,]%*%omega1[i,],ES2[i,]%*%omega2[i,],ES5[i,]%*%omega5[i,])   
  }
  
  if (j == 1) {
    write.table(omega1,"omega1_AL.csv")
    write.table(omega2,"omega2_AL.csv")
    write.table(omega5,"omega5_AL.csv")
    write.table(VaR_Comb,"VaR_AL.csv")
    write.table(ES_Comb,"ES_AL.csv")
  }
  if (j == 2) {
    write.table(omega1,"omega1_NZ.csv")
    write.table(omega2,"omega2_NZ.csv")
    write.table(omega5,"omega5_NZ.csv")
    write.table(VaR_Comb,"VaR_NZ.csv")
    write.table(ES_Comb,"ES_NZ.csv")
  }
  if (j == 3) {
    write.table(omega1,"omega1_FZG.csv")
    write.table(omega2,"omega2_FZG.csv")
    write.table(omega5,"omega5_FZG.csv")
    write.table(VaR_Comb,"VaR_FZG.csv")
    write.table(ES_Comb,"ES_FZG.csv")
  }

}


setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/")

  
