################################################################################## 
## Paper: Forecasting Value-at-Risk and Expected Shortfall of Cryptocurrencies  ##
##        using Combinations based on Jump-Robust and Regime-Switching Models   ##
## Authors: Carlos Trucíos and James W. Taylor
################################################################################## 
#### Implemented by Carlos Trucios
#### VaR Quantile Test of Gaglianone et al. (2011)
################################################################################## 


library(quantreg)
VaR_VQR = function(r,VaR, alpha){
  
  fit1 = suppressWarnings(summary(rq(r ~ VaR, tau = alpha, method = "fn"), method="fn" , se="nid" , cov=TRUE))
  
  a1 = fit1$coefficients[1]
  a2 = fit1$coefficients[2]
  
  M = matrix(nrow = 2 , ncol=1)
  M[1,1] = a1
  M[2,1] = (a2-1)
  
  icov = matrix(nrow = 2 , ncol = 2)
  aa = fit1$cov[1,1]
  bb = fit1$cov[1,2]
  cc = fit1$cov[2,1]
  dd = fit1$cov[2,2]
  icov[2,1] = 1/(bb-aa*dd/cc)
  icov[2,2] = 1/(dd-cc*bb/aa)
  icov[1,1] = -icov[2,1]*dd/cc
  icov[1,2] = -icov[2,2]*bb/aa
  
  statistic = (t(M)) %*% icov %*% M 
  # Added by myself, only in case of computational problems
  if(is.na(statistic)){
    fit1 = suppressWarnings(summary(rq(r ~ VaR, tau = alpha, method = "fn"), method="fn" , se="boot" , cov=TRUE))
    
    a1 = fit1$coefficients[1]
    a2 = fit1$coefficients[2]
    
    M = matrix(nrow = 2 , ncol=1)
    M[1,1] = a1
    M[2,1] = (a2-1)
    
    icov = matrix(nrow = 2 , ncol = 2)
    aa = fit1$cov[1,1]
    bb = fit1$cov[1,2]
    cc = fit1$cov[2,1]
    dd = fit1$cov[2,2]
    icov[2,1] = 1/(bb-aa*dd/cc)
    icov[2,2] = 1/(dd-cc*bb/aa)
    icov[1,1] = -icov[2,1]*dd/cc
    icov[1,2] = -icov[2,2]*bb/aa
    
    statistic = (t(M)) %*% icov %*% M 
  }
  
  p.value = 1-pchisq(statistic[1,1], df=2)
  
  return(p.value)
}

