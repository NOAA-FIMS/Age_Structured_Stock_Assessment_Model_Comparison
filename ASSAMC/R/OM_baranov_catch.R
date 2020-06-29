baranov <- function(F.age, Z.age, N.age, wgt.age=NULL){
  ## INPUT PARAMETERS:
    #F.age = vector of fishing mortality rate at age
    #Z.age = vector of total mortality rate at age including dead discards
    #N.age = vector of abundance at age
    #wgt.age = optional vector of weight at age
  ##OUTPUT PARAMETERS:
    #A list with total landings in numbers and optionally in weight

  L.age.n<-L.age.w<-rep(NA,times=length(N.age))

  L.age.n = F.age*N.age*(1.0-exp(-Z.age))/Z.age
  L.n = sum(L.age.n)

  if (!is.null(wgt.age)){
    L.age.w = wgt.age*L.age.n
    L.w = sum(L.age.w)
  } else {L.w=NA}

  return(list(L.N=L.n, L.W=L.w))
} #end fcn baranov
