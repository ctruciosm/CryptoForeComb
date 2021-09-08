library(gPdtest)

CAViaR_loss = function(ret, param, risklevel, type) {
  n = length(ret)
  Qu = rep(0, n)
  Qu[1] = quantile(ret, risklevel) # Taylor uses res[1:300]
  if (type == "asym_slope") {
    for (i in 2:n) {
      Qu[i] = param[1] + param[2]*Qu[i - 1] + param[3]*max(ret[i - 1], 0) + param[4]*min(ret[i - 1], 0)
    }
  } else {
      for (i in 2:n) {
        Qu[i] = param[1] + param[2]*Qu[i - 1] + param[3]*abs(ret[i - 1])
      }
  }
  loss_function = mean((risklevel - (ret[2:n] < Qu[2:n])) * (ret[2:n] - Qu[2:n]))
  return(loss_function)
}

CAViaR = function(r, risklevel = 0.05, type = "asym_slope") {
  if (type %in% c("asym_slope", "sym_abs")) {
    n = length(r)
    Qu = rep(0, n + 1)
    pini = c(0.01,0.04, 0.5, 0.1)  #?
    params = optim(par = pini, fn = CAViaR_loss, ret = r, risklevel = aux_risklevel, type = type)$par
    Qu[1] = quantile(r, aux_risklevel) 
    if (type == "asym_slope") {
      for (i in 2:(n + 1)) {
        Qu[i] = params[1] + params[2]*Qu[i - 1] + params[3]*max(r[i - 1], 0) + params[4]*min(r[i - 1], 0)
      }
    } else {
      for (i in 2:(n + 1)) {
        Qu[i] = param[1] + param[2]*Qu[i - 1] + param[3]*abs(ret[i - 1])
      }
    }
    return(Qu)
  } else{
    return(print('type option not found, should be either asym_slope or sym_abs.'))
  }
}



r = dailyreturns


CAViaR_EVT = function(r, risklevel = 0.05, type = "asym_slope", theta = 0.075) {
  n = length(r)
  Qu = CAViaR(r, theta, type)
  # Select only exceedances
  u = 0
  std_residual = (r[1:n] - Qu[1:n]) / Qu[1:n]  # r[1:n] - Qu[1:n] are the residuals
  std_res_exceed = std_residual[std_residual > u] 
  params_gpd = gpd.fit(std_res_exceed, method = "amle")
  params_gpd_shape = params_gpd[1] #varepsilon
  params_gpd_scale = params_gpd[2] #beta

  # Eq (7.18) in McNeil, Frey and Embrechts (2005)
  # or Quantile (1-p) in Eq (9.7) of Engle & Manganelli (2004)
  std_res_Qu = u + (params_gpd_scale/params_gpd_shape)*((theta/risklevel)^(params_gpd_shape) - 1)
  
  if (abs(params_gpd_shape) > 10^(-10)) {
    # Eq 7.18 McNeil, Frey and Embrechts (2005) book
    std_res_Qu = u + (params_gpd_scale/params_gpd_shape)*((theta/risklevel)^(params_gpd_shape) - 1)
  } else {
    std_res_Qu = u + params_gpd_scale*log(theta/risklevel)
  }
  ## Eq 9.19 with sigma_t = q_t In Engle & Manganelli (2004)
  ## Equiv to Eq 7.19 in McNeil, Frey and Embrechts (2005) book
  std_res_ES = (std_res_Qu + params_gpd_scale - params_gpd_shape*u)/(1 - params_gpd_shape)
  
  QuFcst = Qu + std_res_Qu*Qu   # As in Engle & Manganelli (2004) pag 132
  ESFcst = Qu + std_res_ES*Qu   # As in Taylo (2019)
  return(cbind(QuFcst, ESFcst))
}















