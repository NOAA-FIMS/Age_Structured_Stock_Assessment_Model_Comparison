popsim<-function(x){
  ## INPUT PARAMETERS:
  #x = a list containing the following
  #nyr = number of yrs to simulate, F = vector of F's each year,
  #ages = vector of ages,
  #nages = number of ages modeled, R0 = unfished equilibrium recruitment, h = steepnes, Phi.0 = spr unfished,
  #M.age = natural mortality ate age,
  #W.mt = weight at age in mt, mat.age = maturity at age, prop.f=proprotion female
  #selex.L = landings selectivity at age,
  #logR.resids = recruitment residuals (lognormal)

  nyr=x$nyr; f=x$f; ages=x$ages; nages=x$nages;
  R0=x$R0; h=x$h; Phi.0=x$Phi.0;
  M.age=x$M.age; W.mt=x$W.mt; mat.age=x$mat.age; prop.f=x$prop.f;
  selex_fleet=x$selex_fleet; logR.resid=x$logR.resid;

  SSB<-biomass.mt<-abundance<-rep(0,nyr) #quantities of interest (annual)

  L.mt<-L.knum<-vector(mode="list", length=fleet_num)
  names(L.mt) <- paste("fleet", 1:fleet_num, sep="")
  names(L.knum) <- paste("fleet", 1:fleet_num, sep="")
  invisible(sapply(1:length(L.mt), function(x) L.mt[[x]] <<- rep(0,nyr)))
  invisible(sapply(1:length(L.knum), function(x) L.knum[[x]] <<- rep(0,nyr)))

  L.age<-vector(mode="list", length=fleet_num)
  names(L.age) <- paste("fleet", 1:fleet_num, sep="")
  invisible(sapply(1:length(L.age), function(x) L.age[[x]] <<- matrix(0, nrow=nyr, ncol=nages)))

  N.age<-FAA<-matrix(0, nrow=nyr, ncol=nages)

  reprod=prop.f*mat.age*W.mt #measure of reproductive capacity at age

  #Initial conditions assumes equilbrium age structure given initial F
  N.pr1=rep(1,nages) #Number of spawners per recruit at age
  Z=f[1]*selex_fleet$fleet1+M.age
  for (a in 1:(nages-1))
  {N.pr1[a+1]=N.pr1[a]*exp(-Z[a])}
  N.pr1[nages]=N.pr1[nages]/(1-exp(-Z[nages])) #Plus group
  Phi.F=sum(N.pr1*reprod) #Spawners per recruit based on mature female biomass

  R.eq=R0*(4*h*Phi.F-(1-h)*Phi.0)/((5*h-1)*Phi.F)
  if (R.eq<1) {R.eq=1}  #Catch numerical possibility that equilibrium R is negative
  N.age[1,]=R.eq*N.pr1


  for (i in 1:(nyr-1)){
    Z=f[i]*selex_fleet$fleet1 + M.age
    FAA[i,]=f[i]*selex_fleet$fleet1

    SSB[i]=sum(N.age[i,]*reprod)
    N.age[(i+1),1]=BH.fcn(S=SSB[i],h=h,R0=R0,Phi.0=Phi.0)*exp(logR.resid[i+1])
    for (a in 1:(nages-1))
    {N.age[(i+1),(a+1)]=N.age[i,a]*exp(-Z[a])} #Abundance at age in each year
    N.age[(i+1),nages]=N.age[(i+1),nages] + N.age[i,nages]*exp(-Z[nages]) #Plus group correction

    L.age$fleet1[i,]=f[i]*selex_fleet$fleet1/(Z)*N.age[i,]*(1-exp(-Z))
    L.knum$fleet1[i]=sum(L.age$fleet1[i,])/1000
    L.mt$fleet1[i]=sum(L.age$fleet1[i,]*W.mt)
    abundance[i]=sum(N.age[i,])
    biomass.mt[i]=sum(N.age[i,]*W.mt)
  }
  Z=f[nyr]*selex_fleet$fleet1 + M.age
  FAA[nyr,]=f[nyr]*selex_fleet$fleet1
  SSB[nyr]=sum(N.age[nyr,]*reprod)
  L.age$fleet1[nyr,]=f[nyr]*selex_fleet$fleet1/(Z)*N.age[nyr,]*(1-exp(-Z))
  L.knum$fleet1[nyr]=sum(L.age$fleet1[nyr,])/1000
  L.mt$fleet1[nyr]=sum(L.age$fleet1[nyr,]*W.mt)
  abundance[nyr]=sum(N.age[nyr,])
  biomass.mt[nyr]=sum(N.age[nyr,]*W.mt)

  selex.D=rep(0,nages) #selex of discards. not used here (set to 0), but required as input for msy calculations
  msy=msy_calcs(steep=h, R0=R0, M=M.age, wgt=W.mt, prop.f=prop.f, selL=selex_fleet$fleet1, selD=selex.D, selZ=selex_fleet$fleet1, mat.f=mat.age, mat.m=NULL, sigma=0, maxF=1.0, step=0.001)

  return(list(year=year, SSB=SSB, abundance=abundance, biomass.mt=biomass.mt, N.age=N.age, L.age=L.age, L.knum=L.knum, L.mt=L.mt, msy=msy, f=f, FAA=FAA))
} #end popsim

#####################################################################################







