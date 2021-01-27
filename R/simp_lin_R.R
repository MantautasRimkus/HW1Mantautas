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

  simp_lin_cpp(x, y)
}
