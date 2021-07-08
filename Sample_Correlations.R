################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### Descriptive Statistics
################################################################################## 

library(tidyverse)
library(robust)

VaR_ES_Individual = function(risklevel, crypto){
  
  if(crypto == "BTC") setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/ETH/")
  if(crypto == "ETH") setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/ETH/")
  if(crypto == "LTC") setwd("/Volumes/CTRUCIOS_SD/ForecastCombinationCrypto/Codes/Resultados/LTC/")

  mu = read.csv("VaR.csv")[,"mu"]
  VaR = as.matrix(read.csv("VaR.csv")[,-1]) + mu
  ES = as.matrix(read.csv("ES.csv")[,-1]) + mu

  #### VaR and ES 1%
  VaR_full = VaR %>% data.frame() %>% 
    dplyr::select(ends_with(risklevel), -contains("MIXTURE"))
  
  ES_full = ES %>% data.frame() %>% 
    dplyr::select(ends_with(risklevel), -contains("MIXTURE"))
  
  return(list(VaR_full, ES_full))
}

cors <- function(df) {
  M <- robust::covRob(as.matrix(df), corr = TRUE, estim = "mcd")
  return(M$cov)
}

formatted_cors <- function(df){
  cors(df) %>% data.frame() %>%
    tibble::rownames_to_column("measure1") %>% 
    pivot_longer(cols = contains(c("GAS","Boot","GARCH")), names_to = "measure2", values_to = "r")
}


risklevels = c("10", "5", "2")
cryptos = c("BTC", "ETH", "LTC")
i = 1
corre = list()
for (crypto in cryptos){
  for(risk in risklevels){
    AUX = VaR_ES_Individual(risk, crypto)
    correaux = rbind(formatted_cors(AUX[[1]]),formatted_cors(AUX[[2]]))
    correaux$risk = rep(risk, nrow(correaux))
    correaux$riskmeasure = c(rep("VaR", nrow(correaux)/2),rep("ES", nrow(correaux)/2))
    correaux$crypto = rep(crypto, nrow(correaux))
    corre[[i]] = correaux
    i = i+1
  }
}

correl_matrix = rbind(corre[[1]], corre[[2]], corre[[3]], corre[[4]],
                      corre[[5]], corre[[6]], corre[[7]], corre[[8]],
                      corre[[9]])




p1 = correl_matrix %>% 
  mutate(measure1 = str_replace_all(measure1, "10", ""), 
         measure2 = str_replace_all(measure2, "10", ""),
         measure1 = str_replace_all(measure1, "5", ""), 
         measure2 = str_replace_all(measure2, "5", ""),
         measure1 = str_replace_all(measure1, "2", ""), 
         measure2 = str_replace_all(measure2, "2", ""),
         risk = factor(risk, levels=c("10","5","2"),
                       labels = c("10% VaR", "5% VaR", "2.5% VaR"))) %>% 
  filter(riskmeasure == "VaR") %>% 
ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r,2))) +
  geom_tile(linejoin = "round") + geom_text() + 
  scale_x_discrete(limits=c("MSGARCH", "GAS", "FIGARCH", "Boot", "AVGARCH")) +
  facet_grid(risk~crypto) + ylab("") + xlab("") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 
  
  


p2 = correl_matrix %>% 
  mutate(measure1 = str_replace_all(measure1, "10", ""), 
         measure2 = str_replace_all(measure2, "10", ""),
         measure1 = str_replace_all(measure1, "5", ""), 
         measure2 = str_replace_all(measure2, "5", ""),
         measure1 = str_replace_all(measure1, "2", ""), 
         measure2 = str_replace_all(measure2, "2", ""),
         risk = factor(risk, levels=c("10","5","2"),
                       labels = c("10% ES", "5% ES", "2.5% ES")), 
         measure1 = factor(measure1), 
         measure2 = factor(measure2)) %>% 
  filter(riskmeasure == "ES") %>% 
  ggplot(aes(x = measure1, y = measure2, fill = r, label = round(r,2))) +
  geom_tile(linejoin = "round") + geom_text() + 
  scale_x_discrete(limits=c("MSGARCH", "GAS", "FIGARCH", "Boot", "AVGARCH")) +
  facet_grid(risk~crypto) + ylab("") + xlab("") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) 

library(patchwork)
p1 | p2

