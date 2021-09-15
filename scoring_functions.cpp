#include<RcppArmadillo.h>
#include<Rmath.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]  
arma::mat QL(arma::mat VaR, arma::vec r, double alpha){
  int n = VaR.n_rows, k = VaR.n_cols;
  arma::mat value(n,k);
  for (int j=0; j<n; j++){
    for (int i=0; i<k; i++){
      if(r[j]<=VaR(j,i)){
        value(j,i) = (alpha-1)*(r[j]-VaR(j,i));
      } else{
        value(j,i) = alpha*(r[j]-VaR(j,i));
      }
    }
  }
  return(value);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]  
arma::mat AL(arma::mat VaR, arma::mat ES, arma::vec r, double alpha){
  int n = VaR.n_rows, k = VaR.n_cols;
  arma::mat value(n,k);
  for (int j=0; j<n; j++){
    for (int i=0; i<k; i++){
      if(r[j]<=VaR(j,i)){
        value(j,i) =((-1/ES(j,i))*(ES(j,i) - VaR(j,i) + (VaR(j,i) - r[j])/alpha) - (-log(-ES(j,i))) + (1-log(1-alpha)));
      } else{
        value(j,i) =((-1/ES(j,i))*(ES(j,i) - VaR(j,i)) - (-log(-ES(j,i))) + (1-log(1-alpha)));
      }
    }
  }
  return(value);
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]  
arma::mat NZ(arma::mat VaR, arma::mat ES, arma::vec r, double alpha){
  int n = VaR.n_rows, k = VaR.n_cols;
  arma::mat value(n,k);
  for (int j=0; j<n; j++){
    for (int i=0; i<k; i++){
      if(r[j]<=VaR(j,i)){
        value(j,i) = ((1/(2*sqrt(-ES(j,i))))*(ES(j,i) - VaR(j,i) + (VaR(j,i) - r[j])/alpha) + sqrt(-ES(j,i)));
      } else{
        value(j,i) = ((1/(2*sqrt(-ES(j,i))))*(ES(j,i) - VaR(j,i)) + sqrt(-ES(j,i)));
      }
    }
  }
  return(value);
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]  
arma::mat FZG(arma::mat VaR, arma::mat ES, arma::vec r, double alpha){
  int n = VaR.n_rows, k = VaR.n_cols;
  arma::mat value(n,k);
  for (int j=0; j<n; j++){
    for (int i=0; i<k; i++){
      if(r[j]<=VaR(j,i)){
        value(j,i) = (1 - alpha)*VaR(j,i) - r[j] + (exp(ES(j,i))/(1+exp(ES(j,i))))*(ES(j,i) - VaR(j,i) + (VaR(j,i) - r[j])/alpha) - log(1+exp(ES(j,i))) + log(2);
      } else{
        value(j,i) = -alpha*VaR(j,i) + (exp(ES(j,i))/(1+exp(ES(j,i))))*(ES(j,i) - VaR(j,i)) - log(1+exp(ES(j,i))) + log(2);
      }
    }
  }
  return(value);
}



