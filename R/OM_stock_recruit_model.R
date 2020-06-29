# Stock-Recruitment Functions

BH.fcn<- function(S,h=0.75,R0=100,Phi.0=1){
  ## Beverton-Holt (B-H) Stock-Recruitment (S-R) Function
  ## INPUT PARAMETERS:
  ##    S = vector or scalar of indepenent variable (spawners)
  ##    h = steepness, default is 0.75
  ##    R0 = recruitment of unfished popn, default is 100
  ##    Phi.0 = spawners per recruit of unfished popn, default is 1
  ## OUTPUT PARAMETERS:
  ##    recruits = values of fcn at S
  recruits=(0.8*R0*h*S)/(0.2*R0*Phi.0*(1-h) + S*(h-0.2))
  return(recruits)
}

Ricker <- function(S,h=0.75,R0=100,Phi.0=1){
  ## Ricker Stock-Recruitment (S-R) Function
  ## INPUT PARAMETERS:
  ##    S = vector or scalar of indepenent variable (spawners)
  ##    h = steepness, default is 0.75
  ##    R0 = recruitment of unfished popn, default is 100
  ##    Phi.0 = spawners per recruit of unfished popn, default is 1
  ## OUTPUT PARAMETERS:
  ##    recruits = values of fcn at S
  recruits=S/Phi.0*exp(h*(1-S/(R0*Phi.0)))
  return(recruits)
}
