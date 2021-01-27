#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List simp_lin_cpp(arma::vec y, arma::vec x) {
   int nx=x.n_elem;
/*
  This is where we check if y and x are numeric and
 if they are of the same length
 */
  double beta0num=0, beta1num=0, denom=0;
  double SSE=0, MSE=0;

  arma::vec coefficients=arma::zeros(2),
    fitted_value=arma::zeros(nx),
    coefficients_std=arma::zeros(2),
    residuals=fitted_value=arma::zeros(nx);

  arma::mat quantilescoef=arma::zeros(2,2);

  /*Calculation of coefficients*/
    beta0num=arma::accu(y)*dot(x,x)-arma::accu(x)*dot(x,y);
    beta1num=nx*dot(x,y) - arma::accu(x)*arma::accu(y);
    denom=double(nx)*dot(x,x) - arma::accu(x)*arma::accu(x);

    coefficients[0]=beta0num/denom;
    coefficients[1]=beta1num/denom;

  /*Calculation of fitted values*/
   fitted_value=coefficients[0]+coefficients[1]*x;

  /* Calculation of SSE */
  SSE=dot(y-fitted_value,y-fitted_value);

  /*Calculation of MSE */
  MSE=SSE/(double(nx)-2);

  /*Standard Errors*/
  coefficients_std[0]=sqrt(MSE*(1/double(nx) + (arma::mean(x)*arma::mean(x))/(denom/nx)));
  coefficients_std[1]=sqrt(MSE/(denom/nx));

  /*Residuals*/
  residuals=y-fitted_value;

  /*Quantiles*/
  quantilescoef(0,0)=coefficients[0]+ R::qt(0.025,double(nx)-2, true, false)*coefficients_std[0];
  quantilescoef(0,1)=coefficients[0]+ R::qt(0.025,double(nx)-2,false, false)*coefficients_std[0];
  quantilescoef(1,0)=coefficients[1]+ R::qt(0.025,double(nx)-2, true, false)*coefficients_std[1];
  quantilescoef(1,1)=coefficients[1]+ R::qt(0.025,double(nx)-2,false, false)*coefficients_std[1];

  /*Return function */
  return Rcpp::List::create(Rcpp::Named("Coefficients")=coefficients,
                            Rcpp::Named("Fitted")=fitted_value,
                            Rcpp::Named("MSE")=MSE,
                            Rcpp::Named("Standard_Error")=coefficients_std,
                            Rcpp::Named("Residuals")=residuals,
                            Rcpp::Named("95pct_conf")=quantilescoef);
}
