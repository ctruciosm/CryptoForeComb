#########################################################
# R Implementation of the Fluctuation test 
# (Giacomini and Rossi; 2010 JAE)
#########################################################

fluct_test = function(loss1, loss2, mu = 0.1, lag_truncate = 1, alpha = 0.05, type = "two_sided", dmv_fullsample = TRUE){
  if (!type %in% c("one_sided", "two_sided")) stop("type must be either 'one_sided' or 'two_sided'")
  if (!alpha %in% c(0.05, 0.1)) stop("significance_level must be either 0.05 or 0.1")
  if (!mu %in% seq(0.1,0.9,0.1)) stop("mu must be in {0.1, 0.2, ..., 0.9}")
    
  P <- length(loss1)
  
  if(type == "one_sided"){
    CV <- data.frame(
      mu = seq(from = 0.1, to = 0.9, by = 0.1), 
      alpha_005 = c(3.176,2.938,2.77,2.624,2.475,2.352,2.248,2.080,1.975),
      alpha_010 = c(2.928,2.676,2.482,2.334,2.168,2.030,1.904,1.740,1.6))
  } else{
    CV <- data.frame(
      mu = seq(from = 0.1, to = 0.9, by = 0.1), 
      alpha_005 = c(3.393, 3.179, 3.012, 2.89, 2.779, 2.634, 2.56, 2.433, 2.248),
      alpha_010 = c(3.17, 2.948, 2.766, 2.626, 2.5, 2.356, 2.252, 2.13, 1.95))
  }
  
  m <- round(mu * P)
  ld <- loss1 - loss2
  dm_num <- dm_den <- rep(0, P - m + 1)
  
  for (jj in m:P) {
    ind <- which(m:P == jj)
    ld_tmp <- ld[(jj - m + 1):jj]
    dm_num[ind] <- mean(ld_tmp)
    dm_den[ind] <- sqrt(murphydiagram:::vHAC(ld_tmp, k = lag_truncate, meth = "Bartlett")$hac/m)
  }
  
  s2hat <- as.numeric(murphydiagram:::vHAC(ld, k = lag_truncate, meth = "Bartlett")$hac)

  dm1 <- dm_num/sqrt(s2hat/m)
  dm2 <- dm_num/dm_den
  if (dmv_fullsample) {
    dm_final <- dm1
  } else {
    dm_final <- dm2
  }

  CVS <- ifelse(alpha == 0.05,
                CV[which(CV$mu == mu),"alpha_005"],
                CV[which(CV$mu == mu),"alpha_010"])

  if (type == "one_sided"){
    return(list(fluc = data.frame(x = m:P, y = dm_final),cv_sup = CVS))
  } else{
    return(list(fluc = data.frame(x = m:P, y = dm_final),cv_sup = CVS, cv_inf = -CVS))
  }
} 

