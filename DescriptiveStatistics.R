################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### Descriptive Statistics
################################################################################## 
library(tidyverse)
library(qrmtools)
library(robts)

# If robust autocorrelations are desired, use:
# bacf <- acfrob(x, plot = FALSE, 50, approach = "RA")$acf  
# bacfdf <- data.frame(lag = 0:50, acf = bacf) 


setwd("./Data")
end_date = "2021-12-22"

BTC = read.csv("BTCUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date) %>% rename(BTC=ret)

ETH = read.csv("ETHUSDT-1d-data.csv") %>% 
  mutate(date = as.Date(timestamp)) %>% arrange(date) %>% mutate(ret = c(0,diff(log(close))*100)) %>% 
  dplyr::select(date, ret) %>% filter(date > "2017-08-17", date < end_date) %>% rename(ETH=ret)

Crypto = BTC %>% left_join(ETH, by = "date") %>% dplyr::select(BTC, ETH) 
Crypto = as.matrix(Crypto) 


Crypto2 = BTC %>% left_join(ETH, by = "date") %>% dplyr::select(date, BTC, ETH) 
Crypto = as.matrix(Crypto) 

#### EAD
library(moments)
library(xtable)

Descriptive = function(ret){
  c(min(ret, na.rm = TRUE), quantile(ret, 0.25, na.rm = TRUE), quantile(ret, 0.5, na.rm = TRUE), 
    mean(ret, na.rm = TRUE), quantile(ret, 0.75, na.rm = TRUE), max(ret, na.rm = TRUE),
    sd(ret, na.rm = TRUE), skewness(ret, na.rm = TRUE), kurtosis(ret, na.rm = TRUE))
}
N = dim(Crypto)[2]
Results = matrix(0, ncol = 9, nrow = N)
for (i in 1:N){
  Results[i, ] = Descriptive(Crypto[,i])
}

colnames(Results) = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "Sd", "Skew", "Kurtosis")
row.names(Results) = colnames(Crypto)
tabela = xtable(Results,caption = "Descriptive statistics of daily returns", digits = 2)

print(tabela, file = "Descriptive.tex", compress = FALSE)

OoS = 700
InS = dim(Crypto)[1] - OoS
Crypto2[InS + 1,]

# Figures

#geom_rect(color = "gray",alpha = 0.008, aes(xmin=lubridate::ymd("2020-03-12"), xmax=lubridate::ymd("2021-05-01"), ymin= -Inf, ymax=Inf)) +
  
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
{

p1_1 = ggplot(BTC, aes(date, BTC)) + geom_line(colour= "green4") +
  xlab(" ") + ylab("Bitcoin") + geom_vline(xintercept = Crypto2[InS+1,"date"], linetype = "dashed") + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  theme_bw() + theme(axis.text.x = element_text(size=10,face="bold")) 

p1_2 = ggplot(ETH, aes(date, ETH)) + geom_line(colour= "green4") +
  xlab(" ") + ylab("Ethereum") + geom_vline(xintercept = Crypto2[InS+1,"date"], linetype = "dashed") + 
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  theme_bw() + theme(axis.text.x = element_text(size=10,face="bold"))
}

######### Autocorrelations Generalised Barlett
{

gamma<-function(x,h){
  n<-length(x)
  h<-abs(h)
  x<-x-mean(x)
  gamma<-sum(x[1:(n-h)]*x[(h+1):n])/n
}
rho<-function(x,h) rho<-gamma(x,h)/gamma(x,0)


x = BTC$BTC
n = length(x)
nlag<- 50 
acf.val<-sapply(c(1:nlag),function(h) rho(x,h))
x2<-x^2
var<-1+(sapply(c(1:nlag),function(h) gamma(x2,h)))/gamma(x,0)^2
band<-sqrt(var/n)
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
bacf <- acf(x, plot = FALSE, 50)  
bacfdf <- with(bacf, data.frame(lag, acf))

p2_1 <- ggplot(data=bacfdf[-1,], mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = "green4") + ylab("Bitcoin")+ 
  ylim(c(-0.15,0.15))+
  geom_line(aes(y = -1.96*band, x = 1:50)) +
  geom_line(aes(y = 1.96*band, x = 1:50)) + theme_bw() + 
  theme(legend.position = "none")

x = ETH$ETH
n = length(x)
nlag<- 50 
acf.val<-sapply(c(1:nlag),function(h) rho(x,h))
x2<-x^2
var<-1+(sapply(c(1:nlag),function(h) gamma(x2,h)))/gamma(x,0)^2
band<-sqrt(var/n)
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
bacf <- acf(x, plot = FALSE, 50)  
bacfdf <- with(bacf, data.frame(lag, acf))

p2_2 <- ggplot(data=bacfdf[-1,], mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = "green4") + ylab("Ethereum")+ 
  ylim(c(-0.15,0.15))+
  geom_line(aes(y = -1.96*band, x = 1:50)) +
  geom_line(aes(y = 1.96*band, x = 1:50)) + theme_bw() + 
  theme(legend.position = "none")
}

######### Autocorrelations Barlett
{
retornos = BTC$BTC
inventories = retornos^2
n <- length(inventories)
mean.inventories <- sum(inventories)/n
# Express the data in deviations from the mean
z.bar <- rep(mean.inventories,n)
deviations <- inventories - z.bar
# Calculate the sum of squared deviations from the mean
squaredDeviations <- deviations^2
sumOfSquaredDeviations <-sum(squaredDeviations)
# Create empty vector to store autocorrelation coefficients
r <- c()
# Use a for loop to fill the vector with the coefficients
for (k in 1:n) {
  ends <- n - k
  starts <- 1 + k
  r[k] <- sum(deviations[1:(ends)]*deviations[(starts):(n)])/sumOfSquaredDeviations
}
# Create empty vector to store Bartlett's standard errors
bart.error <- c()
# Use a for loop to fill the vector with the standard errors
for (k in 1:n) {
  ends <- k-1
  bart.error[k] <- ((1 + sum((2*r[0:(ends)]^2)))^0.5)*(n^-0.5)
}

bacf <- acf(retornos^2, plot = FALSE, 50)  
bacfdf <- with(bacf, data.frame(lag, acf))



p3_1 <- ggplot(data=bacfdf[-1,], mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = "green4") + ylab(expression(Bitcoin^{2}))+ 
  geom_line(aes(y = -1.96*bart.error[1:50], x = 1:50)) +
  geom_line(aes(y = 1.96*bart.error[1:50], x = 1:50)) + theme_bw() + 
  theme(legend.position = "none")

retornos = ETH$ETH
inventories = retornos^2
n <- length(inventories)
mean.inventories <- sum(inventories)/n
# Express the data in deviations from the mean
z.bar <- rep(mean.inventories,n)
deviations <- inventories - z.bar
# Calculate the sum of squared deviations from the mean
squaredDeviations <- deviations^2
sumOfSquaredDeviations <-sum(squaredDeviations)
# Create empty vector to store autocorrelation coefficients
r <- c()
# Use a for loop to fill the vector with the coefficients
for (k in 1:n) {
  ends <- n - k
  starts <- 1 + k
  r[k] <- sum(deviations[1:(ends)]*deviations[(starts):(n)])/sumOfSquaredDeviations
}
# Create empty vector to store Bartlett's standard errors
bart.error <- c()
# Use a for loop to fill the vector with the standard errors
for (k in 1:n) {
  ends <- k-1
  bart.error[k] <- ((1 + sum((2*r[0:(ends)]^2)))^0.5)*(n^-0.5)
}

bacf <- acf(retornos^2, plot = FALSE, 50)  
bacfdf <- with(bacf, data.frame(lag, acf))



p3_2 <- ggplot(data=bacfdf[-1,], mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity", fill = "green4") + ylab(expression(Ethereum^{2}))+ 
  geom_line(aes(y = -1.96*bart.error[1:50], x = 1:50)) +
  geom_line(aes(y = 1.96*bart.error[1:50], x = 1:50)) + theme_bw() + 
  theme(legend.position = "none")

}

pdf("crypto_figures.pdf", paper = "a4r", width = 18, height = 8) 
pushViewport(viewport(layout = grid.layout(2, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1_1, vp = vplayout(1, 1))
print(p2_1, vp = vplayout(1, 2))
print(p3_1, vp = vplayout(1, 3))
print(p1_2, vp = vplayout(2, 1))
print(p2_2, vp = vplayout(2, 2))
print(p3_2, vp = vplayout(2, 3))
dev.off()


# setEPS()
# postscript("crypto_figures.eps", paper = "a4", width = 12, height = 8)
# pushViewport(viewport(layout = grid.layout(2, 3)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(p1_1, vp = vplayout(1, 1))
# print(p2_1, vp = vplayout(1, 2))
# print(p3_1, vp = vplayout(1, 3))
# print(p1_2, vp = vplayout(2, 1))
# print(p2_2, vp = vplayout(2, 2))
# print(p3_2, vp = vplayout(2, 3))
# dev.off()
