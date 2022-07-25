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



