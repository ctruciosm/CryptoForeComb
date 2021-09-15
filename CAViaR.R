library(gPdtest)
library(Rcpp)
sourceCpp("CAViaR.cpp")

CAViaR_loss = function(r, params, risklevel, type) {
  n = length(r)
  Qu = rep(0, n)
  Qu[1] = quantile(r, risklevel) # Taylor uses res[1:300]
  if (type == "asym_slope") {
    for (i in 2:n) {
      Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*max(r[i - 1], 0) + params[4]*min(r[i - 1], 0)
    }
  } else {
      for (i in 2:n) {
        Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*abs(r[i - 1])
      }
  }
  loss_function = mean((risklevel - (r[2:n] < Qu[2:n])) * (r[2:n] - Qu[2:n]))
  return(loss_function)
}

CAViaR = function(r, risklevel = 0.05, type = "sym_abs", par_ini = NULL) {
  if (type %in% c("asym_slope", "sym_abs")) {
    n = length(r)
    Qu = rep(0, n + 1)
    # Grid
    if (type == "asym_slope") {
      pini = matrix(0, ncol = 4, nrow = 10000)
      pini[,1] = -0.01*runif(10000)
      pini[,2] = 0.8 + 0.2*runif(10000)
      pini[,3] = -0.5 + runif(10000)
      pini[,4] = 0.5*runif(10000)
      loss = apply(pini, 1, CAViaR_loss, r, risklevel, type)
    } else{
      pini = matrix(0, ncol = 3, nrow = 10000)
      pini[,1] = -0.01*runif(10000)
      pini[,2] = -runif(10000)
      pini[,3] = 0.4 + 0.3 + runif(10000)
      loss = apply(pini, 1, CAViaR_loss, r, risklevel, type)
    }
    # Optimisation
    smallest = order(loss ,decreasing = F)[1:3]
    params1 = optim(par = pini[smallest[1],], fn = CAViaR_loss, r = r, risklevel = risklevel, type = type)
    params2 = optim(par = pini[smallest[2],], fn = CAViaR_loss, r = r, risklevel = risklevel, type = type)
    params3 = optim(par = pini[smallest[3],], fn = CAViaR_loss, r = r, risklevel = risklevel, type = type)
    best_of_three = order(c(params1$value, params2$value, params3$value),decreasing = F)[1]
    if (best_of_three == 1) params = params1
    if (best_of_three == 2) params = params2
    if (best_of_three == 3) params = params3
    if (!is.null(par_ini)) {
      params4 = optim(par = par_ini, fn = CAViaR_loss, r = r, risklevel = risklevel, type = type)
      if (params4$value < params$value) params = params4
    }
    params = params$par
    Qu[1] = quantile(r, risklevel) 
    if (type == "asym_slope") {
      for (i in 2:(n + 1)) {
        Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*max(r[i - 1], 0) + params[4]*min(r[i - 1], 0)
      }
    } else {
      for (i in 2:(n + 1)) {
        Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*abs(r[i - 1])
      }
    }
    return(list(Qu, params))
  } else{
    return(print('type option not found, should be either asym_slope or sym_abs.'))
  }
}

CAViaR_EVT = function(r, risklevel = 0.05, type = "sym_abs", theta = 0.075, par_ini = NULL) {
  n = length(r)
  caviar_method = CAViaR(r, theta, type, par_ini)
  Qu = caviar_method[[1]]
  u = 0
  std_residual = (r[1:n] - Qu[1:n]) / Qu[1:n]             # r[1:n] - Qu[1:n] are the residuals
  std_res_exceed = std_residual[std_residual > u]         # Select only exceedances
  params_gpd = gpd.fit(std_res_exceed, method = "amle")
  params_gpd_shape = params_gpd[1]                        #varepsilon
  params_gpd_scale = params_gpd[2]                        #beta
  std_res_Qu = u + (params_gpd_scale/params_gpd_shape)*((theta/risklevel)^(params_gpd_shape) - 1)   # Eq (7.18) in McNeil, Frey and Embrechts (2005) or Quantile (1-p) in Eq (9.7) of Engle & Manganelli (2004)
  std_res_ES = (std_res_Qu + params_gpd_scale - params_gpd_shape*u)/(1 - params_gpd_shape)          # Eq (9.19) in Engle & Manganelli (2004) [with sigma_t = q_t] or Eq (7.19) in McNeil, Frey and Embrechts (2005)
  VaR = Qu + std_res_Qu*Qu   # As in Engle & Manganelli (2004) pag 132
  ES = Qu + std_res_ES*Qu    # As in Taylor (2019)
  return(list(cbind(VaR, ES), caviar_method[[2]]))
}

CAViaR_EVT_reg = function(r, risklevel = 0.05, type = "sym_abs", theta = 0.075, par_ini = NULL) {
  n = length(r)
  caviar_method = CAViaR(r, theta, type, par_ini)
  Qu = caviar_method[[1]]
  u = 0
  std_residual = (r[1:n] - Qu[1:n]) / Qu[1:n]             # r[1:n] - Qu[1:n] are the residuals
  std_res_exceed = std_residual[std_residual > u]         # Select only exceedances
  params_gpd = gpd.fit(std_res_exceed, method = "amle")
  params_gpd_shape = params_gpd[1]                        #varepsilon
  params_gpd_scale = params_gpd[2]                        #beta
  std_res_Qu = u + (params_gpd_scale/params_gpd_shape)*((theta/risklevel)^(params_gpd_shape) - 1)   # Eq (7.18) in McNeil, Frey and Embrechts (2005) or Quantile (1-p) in Eq (9.7) of Engle & Manganelli (2004)
  VaR = Qu + std_res_Qu*Qu 
  r_exceed = r[r < VaR[1:n]] 
  VaR_exceed = VaR[r < VaR[1:n]]
  delta = as.numeric(coef(lm(r_exceed ~ VaR_exceed - 1)))
  ES = delta*VaR
  return(list(cbind(VaR, ES), caviar_method[[2]]))
}

CAViaR_reg = function(r, risklevel = 0.05, type = "sym_abs", theta = 0.075, par_ini = NULL) {
  n = length(r)
  caviar_method = CAViaR(r, theta, type, par_ini)
  VaR = caviar_method[[1]]
  r_exceed = r[r < VaR[1:n]] 
  VaR_exceed = VaR[r < VaR[1:n]]
  delta = as.numeric(coef(lm(r_exceed ~ VaR_exceed - 1)))
  ES = delta*VaR
  return(list(cbind(VaR, ES), caviar_method[[2]]))
}

ALD_loss = function(r, params, pgamma, risklevel = 0.05, type = "sym_abs") {
  n = length(r)
  Qu = rep(0, n)
  Qu[1] = quantile(r, risklevel) 
  if (type == "asym_slope") {
    for (i in 2:n) {
      Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*max(r[i - 1], 0) + params[4]*min(r[i - 1], 0)
    }
    ES = (1 + exp(pgamma))*Qu
  } else {
    for (i in 2:n) {
      Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*abs(r[i - 1])
    }
    ES = (1 + exp(pgamma))*Qu
  }
  ll_ald = -mean(log((risklevel - 1)/ES[2:n]) + (r[2:n] - Qu[2:n])*(risklevel - (r[2:n] <= Qu[2:n]))/(risklevel*ES[2:n]))
  return(ll_ald)
}

ALD = function(r, risklevel = 0.05, type = "sym_abs", caviar_params = NULL) {
  if (type %in% c("asym_slope", "sym_abs")) {
    n = length(r)
    Qu = rep(0, n + 1)
    if (is.null(caviar_params)) {
      if (type == "asym_slope") {
        caviar_params = CAViaR(r, risklevel, type = "asym_slope", par_ini = caviar_params)[[2]]
        type_code = 1
      } else{
        caviar_params = CAViaR(r, risklevel, type = "sym_abs", par_ini = caviar_params)[[2]]
        type_code = 0
      }
    } else{
      type_code = ifelse(type == "asym_slope", 1, 0)
    }
    gamma_pini = ALD_grid(r, risklevel, caviar_params, type_code)
    pgamma = optim(par = gamma_pini, fn = ALD_loss, r = r, param = caviar_params, risklevel = risklevel, type = type, method = "Brent", lower = -10, upper = 4)$par
    Qu[1] = quantile(r, risklevel) 
    if (type == "asym_slope") {
      for (i in 2:(n + 1)) {
        Qu[i] = caviar_params[1] + caviar_params[2]*Qu[i - 1] + caviar_params[3]*max(r[i - 1], 0) + caviar_params[4]*min(r[i - 1], 0)
      }
    } else {
      for (i in 2:(n + 1)) {
        Qu[i] = caviar_params[1] + caviar_params[2]*Qu[i - 1] + caviar_params[3]*abs(r[i - 1])
      }
    }
    ES = (1 + exp(pgamma))*Qu
    return(list(cbind(Qu, ES), caviar_params))
  } else{
    return(print('type option not found, should be either asym_slope or sym_abs.'))
  }
}


