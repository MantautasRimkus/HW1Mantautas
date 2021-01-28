#' Simple linear regression
#'
#' Takes two numeric vectors and gives results of linear regression
#'
#' @param y is numeric vector known as response
#' @param x is numeric vector known as predictor. x length must be equal to y length
#'
#' @return Returns regression coefficients, their corresponding standard errors and 95pct confidence intervals, residuals, and predicted values as a list
#'
#' @examples
#' simp_lin_R(seq(1:10), seq(10:1))
#' simp_lin_R(rnorm(1000),rnorm(1000))
#'
#' @author Mantautas Rimkus, \email{mantauc@@colostate.edu}
#' @keywords simple linear regression
#'
#'
#' @export
#' @importFrom Rcpp RcppArmadillo
#'


simp_lin_R <- function(y,x){
  if(!is.numeric(x)) {
    stop("x is not numeric vector")
  }

  if(!is.numeric(y)) {
    stop("y is not numeric vector")
  }

  if(length(x)<=2) {
    stop("x has equal and less than 2 elements")
  }

  if(length(y)<=2) {
    stop("y has equal and less than 2 elements")
  }

  if(length(x)!=length(y)){
    stop("x and y does not have equal lengths")
  }

  simp_lin_cpp(y=y, x=x)
}
