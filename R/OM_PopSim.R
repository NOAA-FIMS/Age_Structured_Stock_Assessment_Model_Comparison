#' @export
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


  stocks <- vector(mode="list", length=length(x))
  names(stocks) <- paste("stock", 1:length(stocks), sep="")

  for (i in 1:length(x)){
    SSB<-biomass.mt<-abundance<-Z<-rep(0,x[[i]]$nyr) #quantities of interest (annual)

    L.mt<-L.knum<-vector(mode="list", length=x[[i]]$fleet_num)
    L.mt <- lapply(1:length(L.mt), function(j) L.mt[[j]] <- rep(0,x[[i]]$nyr))
    names(L.mt) <- paste("fleet", 1:x[[i]]$fleet_num, sep="")
    L.knum <- lapply(1:length(L.knum), function(j) L.knum[[j]] <- rep(0,x[[i]]$nyr))
    names(L.knum) <- paste("fleet", 1:x[[i]]$fleet_num, sep="")

    L.age<-vector(mode="list", length=x[[i]]$fleet_num)
    L.age <- lapply(1:length(L.age), function(j) L.age[[j]] <- matrix(0, nrow=x[[i]]$nyr, ncol=x[[i]]$nages))
    names(L.age) <- paste("fleet", 1:x[[i]]$fleet_num, sep="")

    N.age<-FAA<-matrix(0, nrow=x[[i]]$nyr, ncol=x[[i]]$nages)
    stocks[[i]] <- list(
      SSB=SSB,
      biomass.mt=biomass.mt,
      abundance=abundance,
      L.mt=L.mt,
      L.knum=L.knum,
      L.age=L.age,
      N.age=N.age,
      FAA=FAA,
      Z=Z
    )

    reprod=x[[i]]$proportion.female*x[[i]]$mat.age*x[[i]]$W.mt

    #Initial conditions assumes equilbrium age structure given initial F
    N.pr1=rep(1,x[[i]]$nages) #Number of spawners per recruit at age
    stocks[[i]]$Z=x[[i]]$f[1]*x[[i]]$selex_fleet$fleet1+x[[i]]$M.age
    for (a in 1:(x[[i]]$nages-1))
    {N.pr1[a+1]=N.pr1[a]*exp(-stocks[[i]]$Z[a])}
    N.pr1[nages]=N.pr1[nages]/(1-exp(-stocks[[i]]$Z[nages])) #Plus group
    Phi.F=sum(N.pr1*reprod) #Spawners per recruit based on mature female biomass

    if(x[[i]]$om_bias_cor==TRUE){
      BC <- x[[i]]$logR_sd^2/2
    } else{
      BC <- 0
    }

    if(x[[i]]$om_bias_cor==TRUE & x[[i]]$bias_cor_method=="median_unbiased"){
      if (x[[i]]$SRmodel == 1) {
        R.eq=x[[i]]$R0*(exp(BC)*4*x[[i]]$h*Phi.F-(1-x[[i]]$h)*x[[i]]$Phi.0)/((5*x[[i]]$h-1)*Phi.F)
      }
      if (x[[i]]$SRmodel == 2) {
        R.eq=x[[i]]$R0*(1+log(exp(BC)*Phi.F/x[[i]]$Phi.0)/x[[i]]$h)/(Phi.F/x[[i]]$Phi.0)
      }
    } else {
      if (x[[i]]$SRmodel == 1) {
        R.eq=x[[i]]$R0*(4*x[[i]]$h*Phi.F-(1-x[[i]]$h)*x[[i]]$Phi.0)/((5*x[[i]]$h-1)*Phi.F)
      }
      if (x[[i]]$SRmodel == 2) {
        R.eq=x[[i]]$R0*(1+log(Phi.F/x[[i]]$Phi.0)/x[[i]]$h)/(Phi.F/x[[i]]$Phi.0)
      }

    }

    if (R.eq<1) {R.eq=1}  #Catch numerical possibility that equilibrium R is negative
    N.age[1,]=R.eq*N.pr1
    stocks[[i]]$N.age[1,]=N.age[1,]
  }

  for (i in 1:(x[[1]]$nyr-1)){
    for (j in 1:length(x)) {
      stocks[[j]]$Z=x[[j]]$f[i]*x[[j]]$selex_fleet$fleet1 + x[[j]]$M.age
      stocks[[j]]$FAA[i,]=x[[j]]$f[i]*x[[j]]$selex_fleet$fleet1

      stocks[[j]]$SSB[i]=sum(stocks[[j]]$N.age[i,]*reprod)
      if (x[[j]]$om_bias_cor==TRUE & x[[j]]$bias_cor_method=="mean_unbiased") {
        stocks[[j]]$N.age[(i+1),1]=SRmodel_fcn(
          S=stocks[[j]]$SSB[i],
          h=x[[j]]$h,
          R0=x[[j]]$R0,
          Phi.0=x[[j]]$Phi.0,
          model=x[[j]]$SRmodel)*exp(x[[j]]$logR.resid[i+1])*exp(-BC)
      } else {
        stocks[[j]]$N.age[(i+1),1]=SRmodel_fcn(
          S=stocks[[j]]$SSB[i],
          h=x[[j]]$h,
          R0=x[[j]]$R0,
          Phi.0=x[[j]]$Phi.0,
          model=x[[j]]$SRmodel)*exp(x[[j]]$logR.resid[i+1])
      }

      ## It is inherently assumed that no movement or dispersal of individuals out of the region of birth occurs before age 1. The model could be easily be altered to allow for this by calclating both local and total recruitment for each sub-population, but it requires an additional data source to help inform larval dispersal coefficients estimates.

      for (a in 1:(x[[j]]$nages-1)){
        stocks[[j]]$N.age[(i+1),(a+1)]=stocks[[j]]$N.age[i,a]*exp(-stocks[[j]]$Z[a])
      } #Abundance at age in each year
      stocks[[j]]$N.age[(i+1),nages]=stocks[[j]]$N.age[(i+1),nages] + stocks[[j]]$N.age[i,nages]*exp(-stocks[[j]]$Z[nages]) #Plus group correction

    }

    # Movement
    stocks_movement <- stocks
    for (k in 1:length(stocks)){
      temp <- matrix(NA, nrow=length(stocks), ncol=length(stocks[[k]]$N.age[(i+1),]))
      for (m in 1:nrow(x[[k]]$movement_matrix[[(i+1)]])){
        temp[m, ] <- stocks[[m]]$N.age[(i+1),]*x[[m]]$movement_matrix[[(i+1)]][m,k]
      }
      stocks_movement[[k]]$N.age[(i+1),] <- apply(temp, 2, sum)
    }
    stocks <- stocks_movement

    for (j in 1:length(stocks)){
      stocks[[j]]$L.age$fleet1[i,]=x[[j]]$f[i]*x[[j]]$selex_fleet$fleet1/(stocks[[j]]$Z)*stocks[[j]]$N.age[i,]*(1-exp(-stocks[[j]]$Z))
      stocks[[j]]$L.knum$fleet1[i]=sum(stocks[[j]]$L.age$fleet1[i,])/1000
      stocks[[j]]$L.mt$fleet1[i]=sum(stocks[[j]]$L.age$fleet1[i,]*x[[j]]$W.mt)
      stocks[[j]]$abundance[i]=sum(stocks[[j]]$N.age[i,])
      stocks[[j]]$biomass.mt[i]=sum(stocks[[j]]$N.age[i,]*x[[j]]$W.mt)
    }
  }

  for (i in 1:length(stocks)){
    stocks[[i]]$Z=x[[i]]$f[x[[i]]$nyr]*x[[i]]$selex_fleet$fleet1 + x[[i]]$M.age
    stocks[[i]]$FAA[x[[i]]$nyr,]=x[[i]]$f[x[[i]]$nyr]*x[[i]]$selex_fleet$fleet1
    stocks[[i]]$SSB[x[[i]]$nyr]=sum(stocks[[i]]$N.age[x[[i]]$nyr,]*reprod)
    stocks[[i]]$L.age$fleet1[x[[i]]$nyr,]=x[[i]]$f[x[[i]]$nyr]*x[[i]]$selex_fleet$fleet1/(stocks[[i]]$Z)*stocks[[i]]$N.age[x[[i]]$nyr,]*(1-exp(-stocks[[i]]$Z))
    stocks[[i]]$L.knum$fleet1[x[[i]]$nyr]=sum(stocks[[i]]$L.age$fleet1[x[[i]]$nyr,])/1000
    stocks[[i]]$L.mt$fleet1[x[[i]]$nyr]=sum(stocks[[i]]$L.age$fleet1[x[[i]]$nyr,]*x[[i]]$W.mt)
    stocks[[i]]$abundance[x[[i]]$nyr]=sum(stocks[[i]]$N.age[x[[i]]$nyr,])
    stocks[[i]]$biomass.mt[x[[i]]$nyr]=sum(stocks[[i]]$N.age[x[[i]]$nyr,]*x[[i]]$W.mt)
  }

  # Bilogical reference points calculation
  if (brp_f_option == "independentF") {
    for (i in 1:length(x)){
      x[[i]]$selex.D <- rep(0,x[[i]]$nages) #selex of discards. not used here (set to 0), but required as input for msy calculations
      stocks[[i]]$msy <- msy_calcs_independentF(
        steep=x[[i]]$h,
        R0=x[[i]]$R0,
        M=x[[i]]$M.age,
        wgt=x[[i]]$W.mt,
        prop.f=x[[i]]$proportion.female,
        selL=x[[i]]$selex_fleet$fleet1,
        selD=x[[i]]$selex.D,
        selZ=x[[i]]$selex_fleet$fleet1,
        mat.f=x[[i]]$mat.age,
        mat.m=NULL,
        sigma=x[[i]]$logR_sd,
        om_bias_cor=x[[i]]$om_bias_cor,
        bias_cor_method=x[[i]]$bias_cor_method,
        SRmodel=x[[i]]$SRmodel,
        brp_f_vector=x[[i]]$brp_f_vector)
    }
  }

  if (brp_f_option == "dependentF") {
    f_list <- vector(mode="list", length=length(x))
    for (i in 1:length(x)){
      f_list[[i]] <- x[[i]]$brp_f_vector
    }

    f_combinations <- expand.grid(f_list)

    for (i in 1:length(x)){
      x[[i]]$selex.D <- rep(0,x[[i]]$nages) #selex of discards. not used here (set to 0), but required as input for msy calculations
      stocks[[i]]$msy <- msy_calcs_independentF(
        steep=x[[i]]$h,
        R0=x[[i]]$R0,
        M=x[[i]]$M.age,
        wgt=x[[i]]$W.mt,
        prop.f=x[[i]]$proportion.female,
        selL=x[[i]]$selex_fleet$fleet1,
        selD=x[[i]]$selex.D,
        selZ=x[[i]]$selex_fleet$fleet1,
        mat.f=x[[i]]$mat.age,
        mat.m=NULL,
        sigma=x[[i]]$logR_sd,
        om_bias_cor=x[[i]]$om_bias_cor,
        bias_cor_method=x[[i]]$bias_cor_method,
        SRmodel=x[[i]]$SRmodel,
        brp_f_vector=f_combinations[,i])
    }
  }

  for (i in 1:length(stocks)){

    if(x[[i]]$initial_equilibrium_F==FALSE){
      stocks[[i]]$year<-x[[i]]$year[1]:(x[[i]]$year[length(x[[i]]$year)]-1)
      stocks[[i]]$SSB<-stocks[[i]]$SSB[2:length(stocks[[i]]$SSB)]
      stocks[[i]]$abundance<-stocks[[i]]$abundance[2:length(stocks[[i]]$abundance)]
      stocks[[i]]$biomass.mt<-stocks[[i]]$biomass.mt[2:length(stocks[[i]]$biomass.mt)]
      stocks[[i]]$N.age <- stocks[[i]]$N.age[2:nrow(stocks[[i]]$N.age),]
      stocks[[i]]$L.age$fleet1 <- stocks[[i]]$L.age$fleet1[2:nrow(stocks[[i]]$L.age$fleet1),]
      stocks[[i]]$L.knum$fleet1 <- stocks[[i]]$L.knum$fleet1[2:length(stocks[[i]]$L.knum$fleet1)]
      stocks[[i]]$L.mt$fleet1 <- stocks[[i]]$L.mt$fleet1[2:length(stocks[[i]]$L.mt$fleet1)]
      stocks[[i]]$f <- x[[i]]$f[2:length(x[[i]]$f)]
      stocks[[i]]$FAA <- stocks[[i]]$FAA[2:nrow(stocks[[i]]$FAA),]
    }
  }

  return(list(stocks=stocks))
} #end popsim

#####################################################################################







