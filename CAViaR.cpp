#include<RcppArmadillo.h>
#include<Rmath.h>

using namespace Rcpp;



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP ALD_grid(arma::vec caviar_params, arma::vec r, double risklevel, double type) {
  Rcpp::Function ALD_loss("ALD_loss");
  if(type == 1){
    double pgamma, aux_pgamma, loss, aux_loss;
    NumericVector params_ini(5);
    pgamma = 0.2;
    params_ini[0] = caviar_params[0];
    params_ini[1] = caviar_params[1];
    params_ini[2] = caviar_params[2];
    params_ini[3] = caviar_params[3];
    params_ini[4] = pgamma;
    loss = Rcpp::as<double>(ALD_loss(r, params_ini, risklevel, "asym_slope"));
    for (int i=1; i < 1000; i++) {
      aux_pgamma = 3*Rcpp::as<double>(runif(1));
      params_ini[4] = aux_pgamma;
      aux_loss= Rcpp::as<double>(ALD_loss(r, params_ini, risklevel, "asym_slope"));
      if (aux_loss < loss){
        loss = aux_loss;
        pgamma = aux_pgamma;
      }
    }
    return Rcpp::wrap(pgamma);
  } else{
    double pgamma, aux_pgamma, loss, aux_loss;
    NumericVector params_ini(4);
    pgamma = 0.2;
    params_ini[0] = caviar_params[0];
    params_ini[1] = caviar_params[1];
    params_ini[2] = caviar_params[2];
    params_ini[3] = pgamma;
    loss = Rcpp::as<double>(ALD_loss(r, params_ini, risklevel, "sym_abs"));
    for (int i=1; i < 1000; i++) {
      aux_pgamma = -3*Rcpp::as<double>(runif(1));
      params_ini[3] = aux_pgamma;
      aux_loss= Rcpp::as<double>(ALD_loss(r, params_ini, risklevel, "sym_abs"));
      if (aux_loss < loss){
        loss = aux_loss;
        pgamma = aux_pgamma;
      }
    }
    return Rcpp::wrap(pgamma);
  }
}




// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP CAViaR_loss(arma::vec params, arma::vec r, double risklevel, double type) {
  Rcpp::Function quantile("quantile");
  Rcpp::Function max("max");
  Rcpp::Function min("min");
  Rcpp::Function abs("abs");
  Rcpp::Function mean("mean");
  int n = r.size();
  arma::vec Qu(n);
  arma::vec aux(n-1);

  Qu[0] = Rcpp::as<double>(quantile(r, risklevel));
  if (type == 1) {
    for (int i=1; i < n; i++) {
      Qu[i] = params[0] + params[1]*Qu[i - 1] + params[2]*Rcpp::as<double>(max(r[i - 1], 0)) + params[3]*Rcpp::as<double>(min(r[i - 1], 0));
      if (r[i] < Qu[i]) {
        aux[i-1] = (risklevel - 1)*(r[i] - Qu[i]);
      } else {
        aux[i-1] = risklevel*(r[i] - Qu[i]);
      }
    }
  } else {
    for (int i=1; i < n; i++) {
      Qu[i] = params[0] + params[1]*Qu[i - 1] + params[2]*Rcpp::as<double>(abs(r[i - 1]));
      if (r[i] < Qu[i]) {
        aux[i-1] = (risklevel - 1)*(r[i] - Qu[i]);
      } else {
        aux[i-1] = risklevel*(r[i] - Qu[i]);
      }
    }
  }
  return Rcpp::wrap(Rcpp::as<double>(mean(aux)));
}


