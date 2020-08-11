# Stock-Recruitment Functions

SRmodel_fcn<- function(S,h=0.75,R0=100,Phi.0=1,model=1){
  ## Beverton-Holt (B-H) Stock-Recruitment (S-R) Function
  ## INPUT PARAMETERS:
  ##    S = vector or scalar of indepenent variable (spawners)
  ##    h = steepness, default is 0.75
  ##    R0 = recruitment of unfished popn, default is 100
  ##    Phi.0 = spawners per recruit of unfished popn, default is 1
  ## OUTPUT PARAMETERS:
  ##    recruits = values of fcn at S
  if (model == 1) {
    recruits=(0.8*R0*h*S)/(0.2*R0*Phi.0*(1-h) + S*(h-0.2))
  }

  if (model == 2) {
    recruits=S/Phi.0*exp(h*(1-S/(R0*Phi.0)))
  }
  return(recruits)
}

