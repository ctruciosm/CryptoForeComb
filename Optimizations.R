### Optimizations
#JSF = function(VaR, ES, r, alpha){
#  (ifelse(r<= VaR,1,0) - alpha)*G1(VaR) 
#  - ifelse(r<=VaR,1,0)*G1(r)
#  + G2(ES)*(ES - VaR + ifelse(r<=VaR,1,0)*(VaR - r)/alpha) 
#  - G3(ES) + a(r)
#}

# Scoring Functions


QL = function(VaR, r, alpha){
  D = dim(VaR)
  if (is.null(D)){
    k = 1
    val_ =  (alpha - ifelse(r<=VaR,1,0))*(r-VaR)
  } else {
    k = D[2]
    n = D[1]
    val_ = matrix(0,ncol = k, nrow = n)
    for (i in 1:k){
      val_[,i] = (alpha - ifelse(r<=VaR[,i],1,0))*(r-VaR[,i])
    }
  }
  return(val_)
}

AL = function(VaR, ES, r, alpha){
  D = dim(VaR)
  if (is.null(D)){
    k = 1
    val_ = ((-1/ES)*(ES - VaR + ifelse(r<=VaR,1,0)*(VaR - r)/alpha) - (-log(-ES)) + (1-log(1-alpha)))
  } else {
    k = D[2]
    n = D[1]
    val_ = matrix(0,ncol = k, nrow = n)
    for (i in 1:k){
    val_[,i] = ((-1/ES[,i])*(ES[,i] - VaR[,i] + ifelse(r<=VaR[,i],1,0)*(VaR[,i] - r)/alpha) - (-log(-ES[,i])) + (1-log(1-alpha)))
    }
  }
  return(val_)
}

FZ0 = function(VaR, ES, r, alpha){
  D = dim(VaR)
  if (is.null(D)){
    k = 1
    val_ = ((-1/ES)*(ES - VaR + ifelse(r<=VaR,1,0)*(VaR - r)/alpha) - (-log(-ES)))
  } else {
    k = D[2]
    n = D[1]
    val_ = matrix(0,ncol = k, nrow = n)
    for (i in 1:k){
      val_[,i] = ((-1/ES[,i])*(ES[,i] - VaR[,i] + ifelse(r<=VaR[,i],1,0)*(VaR[,i] - r)/alpha) - (-log(-ES[,i])))
    }
  }
  return(val_)
}

NZ = function(VaR, ES, r, alpha){
  D = dim(VaR)
  if (is.null(D)){
    k = 1
    val_ = ((1/(2*sqrt(-ES)))*(ES - VaR + ifelse(r<=VaR,1,0)*(VaR - r)/alpha) + sqrt(-ES))
  } else {
    k = D[2]
    n = D[1]
    val_ = matrix(0,ncol = k, nrow = n)
    for (i in 1:k){
      val_[,i] = ((1/(2*sqrt(-ES[,i])))*(ES[,i] - VaR[,i] + ifelse(r<=VaR[,i],1,0)*(VaR[,i] - r)/alpha) + sqrt(-ES[,i]))
    }
  }
  return(val_)
}

FZG = function(VaR, ES, r, alpha){
  D = dim(VaR)
  if (is.null(D)){
    k = 1
    val_ = ((ifelse(r<= VaR,1,0) - alpha)*VaR - ifelse(r<=VaR,1,0)*r+ (exp(ES)/(1+exp(ES)))*(ES - VaR + ifelse(r<=VaR,1,0)*(VaR - r)/alpha) - (log(1+exp(ES))) + log(2))
  } else {
    k = D[2]
    n = D[1]
    val_ = matrix(0,ncol = k, nrow = n)
    for (i in 1:k){
      val_[,i] = ((ifelse(r<= VaR[,i],1,0) - alpha)*VaR[,i] - ifelse(r<=VaR[,i],1,0)*r+ (exp(ES[,i])/(1+exp(ES[,i])))*(ES[,i] - VaR[,i] + ifelse(r<=VaR[,i],1,0)*(VaR[,i] - r)/alpha) - (log(1+exp(ES[,i]))) + log(2))
    }
  }
  return(val_)
}


RSC = function(lambda, VaR, ES, r, alpha, S = JSF){
  if(length(r) == 1){
    M = length(VaR) 
    VaR = matrix(VaR, ncol = M, nrow = 1)
    ES = matrix(ES, ncol = M, nrow = 1)
  } else {
    if(is.data.frame(VaR)) VaR = as.matrix(VaR)
    if(is.data.frame(ES)) ES = as.matrix(ES)
    M = dim(VaR)[2]
  }

  omega =  matrix(0,ncol = 1, nrow = M)
  for (i in 1:M){
    omega[i,1] = exp(-lambda * sum(S(VaR[,i], ES[,i], r, alpha)))/sum( exp(-lambda * apply(S(VaR, ES, r, alpha),2,sum) ))
  }

  if(!is.nan(sum(sum(omega)))) {
    VaR_c = VaR%*%omega
    ES_c = ES%*%omega
    return(sum(S(VaR_c, ES_c, r, alpha)))
  } else{
    return(Inf)
  }
}

RSC_Eval = function(lambda, VaR, ES, r, alpha, S = JSF){
  if(length(r) == 1){
    M = length(VaR) 
    VaR = matrix(VaR, ncol = M, nrow = 1)
    ES = matrix(ES, ncol = M, nrow = 1)
  } else {
    if(is.data.frame(VaR)) VaR = as.matrix(VaR)
    if(is.data.frame(ES)) ES = as.matrix(ES)
    M = dim(VaR)[2]
  }
  
  omega =  matrix(0,ncol = 1, nrow = M)
  for (i in 1:M){
    omega[i,1] = exp(-lambda * sum(S(VaR[,i], ES[,i], r, alpha)))/sum( exp(-lambda * apply(S(VaR, ES, r, alpha),2,sum) ))
  }
  
return(omega)
}

RSC_grid = function(VaR, ES, r, alpha, S = JSF){
  lambda = seq(from = 0.01, to = 1000, length.out = 10^3)
  val = c()
  for (i in 1:(length(lambda))){
    val[i] = RSC(lambda[i], VaR, ES, r, alpha, S)
  }
  #return(lambda[which(val == Inf)[1]-1])
  return(c(lambda[which(val == Inf)[1]-1],lambda[which(val == min(val))[1]]))
}




