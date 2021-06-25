###########################################
### Forecasting Combination VaR Figure  ###
###########################################


setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/BTC/")
mu = read.csv("VaR.csv")[,"mu"]
VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
VaR_AL = read.csv("VaR_AL.csv",sep = " ")[,]
ret = read.csv("VaR.csv")[,"OoS"]
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], AL = VaR_AL[,1], ret)
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], AL = VaR_AL[,2], ret)
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], AL = VaR_AL[,3], ret)
VaRBTC = rbind(VaR1, VaR2, VaR5)
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/ETH/")
mu = read.csv("VaR.csv")[,"mu"]
VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
VaR_AL = read.csv("VaR_AL.csv",sep = " ")[,]
ret = read.csv("VaR.csv")[,"OoS"]
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], AL = VaR_AL[,1], ret)
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], AL = VaR_AL[,2], ret)
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], AL = VaR_AL[,3], ret)
VaRETH = rbind(VaR1, VaR2, VaR5)
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/LTC/")
mu = read.csv("VaR.csv")[,"mu"]
VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
VaR_AL = read.csv("VaR_AL.csv",sep = " ")[,]
ret = read.csv("VaR.csv")[,"OoS"]
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], AL = VaR_AL[,1], ret)
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], AL = VaR_AL[,2], ret)
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], AL = VaR_AL[,3], ret)
VaRLTC = rbind(VaR1, VaR2, VaR5)
setwd("/Users/ctruciosm/Dropbox/Academico/ForecastCombinationCrypto/Codes/LAST7/XRP/")
mu = read.csv("VaR.csv")[,"mu"]
VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
VaR_AL = read.csv("VaR_AL.csv",sep = " ")[,]
ret = read.csv("VaR.csv")[,"OoS"]
VaR1 = cbind(VaR[,c("GAS1","MSGARCH1","Boot1")], AL = VaR_AL[,1], ret)
VaR2 = cbind(VaR[,c("GAS2","MSGARCH2","Boot2")], AL = VaR_AL[,2], ret)
VaR5 = cbind(VaR[,c("GAS5","MSGARCH5","Boot5")], AL = VaR_AL[,3], ret)
VaRXRP = rbind(VaR1, VaR2, VaR5)


VaR = rbind(VaRBTC, VaRETH, VaRLTC, VaRXRP)
coins = c(rep("BTC",1095), rep("ETH", 1095), rep("LTC", 1095), rep("XRP", 1095))
risklevels = rep(c(rep("1 %",365), rep("2.5 %", 365), rep("5 %", 365)),4)
x = rep(rep(1:365,3),4)
VaR = data.frame(VaR, risklevels,x, coins)
colnames(VaR) = c("GAS", "MSGARCH", "RGARCHBoot", "AL", "returns", "risklevels", "x", "coins")

library(ggplot2)
figure = ggplot(VaR) + geom_line(aes(x,returns), colour = "#666666", size = 1) + 
  geom_line(aes(x,GAS), colour = "green4", linetype = "longdash") +
  geom_line(aes(x,MSGARCH), colour = "blue4", linetype = "dashed") +
  geom_line(aes(x,RGARCHBoot), colour = "red4", linetype = "twodash") +
  geom_line(aes(x,AL), colour = "black", linetype = "solid") +
  scale_x_continuous(labels = c("2019-06", "2019-09", "2019-12", "2020-03"), breaks = c(0,100,200,300)) +
  facet_grid(risklevels~coins) + xlab(" ")+ xlim(c(270,300)) + 
  theme_bw()+
  theme(axis.text.x = element_text(size = 6)) 
  

setEPS()
postscript("VaR.eps", paper = "a4", width = 20, height = 4)
figure
dev.off()
