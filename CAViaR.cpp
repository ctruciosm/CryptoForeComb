#include<RcppArmadillo.h>
#include<Rmath.h>

using namespace Rcpp;



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP ALD_grid(arma::vec r, double risklevel, arma::vec caviar_params, double type) {
  Rcpp::Function ALD_loss("ALD_loss");
  if(type == 1){
    double pgamma, aux_pgamma, loss, aux_loss;
    pgamma = 0.2;
    loss = Rcpp::as<double>(ALD_loss(r, caviar_params, pgamma, risklevel, "asym_slope"));
    for (int i=1; i < 1000; i++) {
      aux_pgamma = 3*Rcpp::as<double>(runif(1));
      aux_loss= Rcpp::as<double>(ALD_loss(r, caviar_params, aux_pgamma, risklevel, "asym_slope"));
      if (aux_loss < loss){
        loss = aux_loss;
        pgamma = aux_pgamma;
      }
    }
    return Rcpp::wrap(pgamma);
  } else{
    double pgamma, aux_pgamma, loss, aux_loss;
    pgamma = 0.2;
    loss = Rcpp::as<double>(ALD_loss(r, caviar_params, pgamma, risklevel, "sym_abs"));
    for (int i=1; i < 1000; i++) {
      aux_pgamma = -3*Rcpp::as<double>(runif(1));
      aux_loss= Rcpp::as<double>(ALD_loss(r, caviar_params, aux_pgamma, risklevel, "sym_abs"));
      if (aux_loss < loss){
        loss = aux_loss;
        pgamma = aux_pgamma;
      }
    }
    return Rcpp::wrap(pgamma);
  }
}



