## Functions:
# geomean: compute geometric mean of a time series x
# logistic: compute different logistic patterns
# pow: compute power transformation


geomean<-function(x){
 ##compute geometric mean of a time series x
  nx=length(x)
  y=prod(x)^(1/nx)
  return(y)
}

logistic<-function(pattern, x, a1=100, b1=0, a2=100, b2=0){
  ## INPUT PARAMETERS:
  ##    pattern: 1. simple logistic pattern
  ##             2. double logisitc pattern
  ##    x = vector or scalar of indepenent variable
  ##    a = slope parameter, default is 100 ("knife edge" for most applications)
  ##    b = location parameter: x giving 50% probability, default is 0
  ## OUTPUT PARAMETERS:
  ##    P = values of fcn at x
  if (pattern==1) {
    P=1/(1+exp(-a1*(x-b1)))
  }
  if (pattern==2) {
    P_initial=(1/(1+exp(-a1*(x-b1))))*(1-(1/(1+exp(-a2*(x-b2)))))
    P=P_initial/max(P_initial)
  }
  return(P)
}

pow<-function(x,a,b){
  ## INPUT PARAMETERS:
  ##    x = vector or scalar of indepenent variable
  ##    a = coefficient
  ##    b = exponent
  ## OUTPUT PARAMETERS:
  ##    y = values of fcn at x

  y=a*x^b
  return(y)
}
