## A collection of functions:
# geomean: compute geometric mean of a time series x
# logistic: compute different logistic patterns
# pow: compute power transformation

#' Compute geometric mean of a time series x.
#'
#' @param x A number.
#' @return The geomean: \code{y}.
#' @examples
#' geomean(x=c(1,2,3,4,5))
#' geomean(x=rnorm(n=100, mean=100, sd=30))
#' @export
#'
geomean<-function(x){
  nx <- length(x)
  y <- prod(x)^(1/nx)
  return(y)
}


#' Compute various logistic patterns
#'
#' @param pattern logistic function option.
#' Pattern 1. simple logistic pattern
#' Pattern 2. double logisitc pattern
#' @param x Vector or scalar of indepenent variable
#' @param a1 Slope parameter
#' @param b1 Location parameter
#' @param a2 Slope parameter for pattern 2
#' @param b2 Location parameter for pattern 2
#' @return Values of fcn at x: \code{P}.
#' @examples
#' logistic(pattern=1, x=c(1:12), a1=3, b1=1.5)
#' logistic(pattern=2, x=c(1:12), a1=3, b1=1.5, a2=2, b1=10)
#' @export
#'
logistic<-function(pattern, x, a1=NULL, b1=NULL, a2=NULL, b2=NULL){
  if (pattern==1) {
    P <- 1/(1+exp(-a1*(x-b1)))
  }
  if (pattern==2) {
    P_initial <- (1/(1+exp(-a1*(x-b1))))*(1-(1/(1+exp(-a2*(x-b2)))))
    P <- P_initial/max(P_initial)
  }
  return(P)
}

#' Compute power transformation
#'
#' @param x Vector or scalar of indepenent variable.
#' @param a Coefficient
#' @param b Exponent
#' @return Values of fcn at x: \code{y}.
#' @examples
#' pow(x=2, a=3, b=4)
#' pow(x=c(2,3,4), a=3, b=4)
#' @export
#'
pow<-function(x,a,b){
  y <- a*x^b
  return(y)
}
