maindir="C:/Users/bai.li/Desktop/mcp_results_r/cases/case6_3_OM_SS"
i=1
subdir = "functions"
setwd(file.path(maindir, subdir))
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)

msy_calcs<-function(steep, R0, M, wgt, prop.f=0.5, selL, selD, selZ, mat.f=NULL, mat.m=NULL, BC=BC, maxF=1.0, step=0.01, verbose=FALSE){
  
  ## Language:          R
  ## Contains:          Function "msy.calcs"
  ## Programmer:        Kyle Shertzer
  ## First coded:       March, 2006
  ## Last revised:      March, 2006
  ## Purpose:           Compute MSY benchmarks
  ##
  ## Note: Benchmarks based on Bev-Holt S-R fcn with steepness parameterization.
  ##        SSB may be based on females only, males only, or both.
  ##        The measure is controlled by which maturity
  ##        vectors are supplied (mat.f, mat.m, or both).
  ##
  ## INPUT PARAMETERS:
  ##    steep = steepness parameter
  ##    R0    = virgin recruitment parameter
  ##    M     = natural mortality, may be constant or vector
  ##    wgt   = vector of weight at age
  ##    prop.f= proportion female at age, default = 0.5
  ##    selL  = selectivity at age for landings
  ##    selD  = selectivity at age for dead discards
  ##    selZ  = selectivity at age for dead fish
  ##    OPTIONAL INPUT
  ##    mat.f = vector of proportion females mature at age
  ##    mat.m = vector of proportion males mature at age
  ##    sigma = lognormal bias correction -- exp(sigma^2/2)
  ##    maxF  = maximum F examined
  ##    step  = accuracy in MSY calculations (default is 0.01)
  ##
  ## OUTPUT:
  ##    MSY   = maximum sustainable yield
  ##    Dmsy  = dead discards at MSY
  ##    Fmsy  = fishing rate at MSY
  ##    SSBmsy= spawning stock biomass at msy
  ##    Rmsy  = equilibrium recruitment at msy
  ##    Bmsy  = total biomass (male and female) at msy
  ##    Emsy  = exploitation rate at msy (total catch / number of fish)
  ##    spr_msy= spawners per recruit at msy
  ##    SPRmsy= spawning potential ratio (spr_msy/spr_virgin)
  
  #  if (! is.numeric(amin)) stop ("Non-numeric value for minimum age!")
  #  if (! is.numeric(amax)) stop ("Non-numeric value for maximum age!")
  if (! is.numeric(steep)) stop ("Non-numeric value for steepnes!")
  if (! is.numeric(R0)) stop ("Non-numeric value for R0!")
  if (! is.numeric(M)) stop ("Non-numeric value for M!")
  if (! is.numeric(wgt)) stop ("Non-numeric value for weight at age!")
  if (! is.numeric(prop.f)) stop ("Non-numeric value for proportion female at age!")
  if (! is.numeric(selL)) stop ("Non-numeric value for selectivity at age!")
  if (is.null(mat.f) & is.null(mat.m)) stop ("Specify maturity schedule -- male, female, or both!")
  
  if (verbose){
    if (BC == 1) {cat("*** MSY NOTE: Estimates contain no bias correction.\n")}
    else {cat("*** NOTE: Estimates contain bias correction.\n")}
    if (is.null(mat.f)){cat("*** MSY NOTE: SSB based on males only.\n")}
    if (is.null(mat.m)){cat("*** MSY NOTE: SSB based on females only.\n")}
    if (!is.null(mat.f) & !is.null(mat.m)){cat("*** MSY NOTE: SSB based on both sexes.\n")}
  }
  ##INITIALIZATION
  BC=BC              #multiplicative bias correction
  nages=length(wgt)
  if (length(M)>1){M_age=M}        #natural mortality at age (may be constant)
  else M_age=rep(M,nages)
  prop.m=1.0-prop.f                #proportion male
  
  mu.f=rep(0.0,nages)              #proportion females mature at age, initialized at zero
  mu.m=rep(0.0,nages)              #proportion males mature at age, initialized at zero
  if (is.null(mat.f)){mu.m=mat.m}
  if (is.null(mat.m)){mu.f=mat.f}
  if (!is.null(mat.f) & !is.null(mat.m)){mu.f=mat.f; mu.m=mat.m}
  reprod=wgt*(prop.f*mu.f + prop.m *mu.m) #constant vector multiplied by abundance to get SSB at age
  
  
  f_seq=seq(0.0,maxF, by=step)
  spr=rep(0.0,length(f_seq))   #equilibrium spr at F
  S_eq=rep(0.0,length(f_seq))  #equilibrium SSB at F
  R_eq=rep(0.0,length(f_seq))  #equilibrium recruitment at F
  B_eq=rep(0.0,length(f_seq))  #equilibrium biomass at F
  L_eq=rep(0.0,length(f_seq))  #equilibrium landings at F
  D_eq=rep(0.0,length(f_seq))  #equilibrium dead discards at F
  E_eq=rep(0.0,length(f_seq))  #equilibrium exploitation rate at F (landings only)
  
  L_age=rep(0.0,nages)     #landings at age
  D_age=rep(0.0,nages)     #dead discards at age
  F_age=rep(0.0,nages)     #F at age
  Z_age=rep(0.0,nages)     #Z at age
  
  ## Compute virgin spr
  N0=rep(1.0,times=nages)
  for (iage in 2:nages){N0[iage]=N0[iage-1]*exp(-1.0*M_age[iage-1])}
  N0[nages]=N0[nages-1]*exp(-1.*M_age[nages-1])/(1.-exp(-1.0*M_age[nages]))
  spr_F0=sum(N0*reprod)
  
  ## BEGIN ALGORITHM
  for (i in 1:length(f_seq)){
    FL_age=f_seq[i]*selL
    FD_age=f_seq[i]*selD
    Z_age=M_age+f_seq[i]*selZ
    
    N_age=rep(1.0,nages)     #N at age
    for (iage in 2:nages){
      N_age[iage]=N_age[iage-1]*exp(-1.0*Z_age[iage-1])
    }
    #last age is pooled
    N_age[nages]=N_age[nages-1]*exp(-1.*Z_age[nages-1])/
      (1.-exp(-1.0*Z_age[nages]));
    
    
    spr[i]=sum(N_age*reprod) 
    R_eq[i]=(R0/((5.0*steep-1.0)*spr[i]))*
      (BC*4.0*steep*spr[i]-spr_F0*(1.-steep))
    if (R_eq[i]<0.0000001) R_eq[i]=0.0000001        
    
    N_age=R_eq[i]*N_age
    
    S_eq[i]=sum(N_age*reprod)
    B_eq[i]=sum(N_age*wgt)
    
    for(iage in 1:nages){
      L_age[iage]=N_age[iage]*
        (FL_age[iage]/Z_age[iage])*(1.-exp(-1.0*Z_age[iage]))
      D_age[iage]=N_age[iage]*
        (FD_age[iage]/Z_age[iage])*(1.-exp(-1.0*Z_age[iage]))
    }
    L_eq[i]=sum(L_age*wgt)
    D_eq[i]=sum(D_age*wgt)
    E_eq[i]=sum(L_age)/sum(N_age)
    
  } #END F loop
  
  msy_out=max(L_eq)
  F_msy_out=f_seq[L_eq==msy_out]
  spr_msy_out=spr[L_eq==msy_out]
  SR_msy_out=spr_msy_out/spr_F0
  D_msy_out=D_eq[L_eq==msy_out]
  R_msy_out=R_eq[L_eq==msy_out]
  S_msy_out=S_eq[L_eq==msy_out]
  B_msy_out=B_eq[L_eq==msy_out]
  E_msy_out=E_eq[L_eq==msy_out]
  
  if (F_msy_out==maxF){cat("*** Fmsy reached a bound.\n")}
  
  return(list(msy=msy_out, Fmsy=F_msy_out, Dmsy=D_msy_out, spr_msy=spr_msy_out, SPRmsy=SR_msy_out, SSBmsy=S_msy_out, Rmsy=R_msy_out, Bmsy=B_msy_out, Emsy=E_msy_out, f_seq=f_seq, L_eq=L_eq, D_eq=D_eq, SSB_eq=S_eq, R_eq=R_eq))
  
}

parameter_values <- matrix(NA, nrow=6, ncol=5)

#### OM ####
subdir="OM"
load(file.path(maindir, "output", subdir, paste("OM", i, ".RData", sep="")))

om_msy=msy_calcs(steep=om_input$h, R0=om_input$R0, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=om_input$selex_fleet$fleet1, selD=rep(0,om_input$nages), selZ=om_input$selex_fleet$fleet1, mat.f=om_input$mat.age, mat.m=NULL, BC=1, maxF=1.0, step=0.00001)

om_msy$R_eq[1]
om_msy$msy; om_msy$Fmsy; om_msy$SSBmsy
om_output$msy$msy; om_output$msy$Fmsy; om_output$msy$SSBmsy

alpha=4*om_input$R0*om_input$h/(5*om_input$h-1)
beta=om_input$Phi.0*om_input$R0*(1-om_input$h)/(5*om_input$h-1)
om_phif <- (om_output$SSB[1]+beta)/alpha
om_req <- om_output$SSB[1]/((om_output$SSB[1]+beta)/alpha)

parameter_values[,1] <- c(om_input$R0,
                          om_input$Phi.0,
                          om_phif,
                          om_req,
                          om_output$msy$spr_msy,
                          om_output$msy$Rmsy)
om_rexp <- BH.fcn(S=om_output$SSB, h=om_input$h, R0=om_input$R0, Phi.0 = om_input$Phi.0)
om_rexp[1:29]*exp(-0.18)*exp(om_input$logR.resid)[2:30]
om_output$N.age[2:30,1]

write.csv(om_output$SSB, file="C:\\Users\\bai.li\\Desktop\\ssb.csv")
write.csv(om_rexp, file="C:\\Users\\bai.li\\Desktop\\exp_r.csv")
write.csv(om_rexp*exp(-0.18), file="C:\\Users\\bai.li\\Desktop\\exp_r_bc.csv")
write.csv(exp(om_input$logR.resid), file="C:\\Users\\bai.li\\Desktop\\exp_resid.csv")
write.csv(om_output$N.age[,1], file="C:\\Users\\bai.li\\Desktop\\pred_r.csv")

#### AMAK ####
subdir = "AMAK"
setwd(file.path(maindir, "output", subdir, paste("s", i, sep="")))
amak_output <- readRep("For_R", suffix = ".rep")
amak_std <- readRep("amak", suffix = ".std")

#amak_msy=msy_calcs(steep=om_input$h, R0=exp(amak_std$value[which(amak_std$name=="log_Rzero")]), M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=amak_output$sel_fsh_1[1,3:ncol(amak_output$sel_fsh_1)], selD=rep(0,om_input$nages), selZ=amak_output$sel_fsh_1[1,3:ncol(amak_output$sel_fsh_1)], mat.f=om_input$mat.age, mat.m=NULL, BC=1, maxF=1.0, step=0.00001)
amak_msy=msy_calcs(steep=om_input$h, R0=exp(amak_std$value[which(amak_std$name=="log_Rzero")]), M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=amak_output$sel_fsh_1[1,3:ncol(amak_output$sel_fsh_1)], selD=rep(0,om_input$nages), selZ=amak_output$sel_fsh_1[1,3:ncol(amak_output$sel_fsh_1)], mat.f=om_input$mat.age, mat.m=NULL, BC=exp(0.6^2/2), maxF=1.0, step=0.00001)

amak_msy$R_eq[1]
om_msy$msy; om_msy$Fmsy; om_msy$SSBmsy
amak_msy$msy; amak_msy$Fmsy; amak_msy$SSBmsy
amak_std$value[which(amak_std$name=="MSY")]; amak_std$value[which(amak_std$name=="Fmsy")]; amak_std$value[which(amak_std$name=="Bmsy")]

alpha=4*exp(amak_std$value[which(amak_std$name=="log_Rzero")])*om_input$h/(5*om_input$h-1)
beta=amak_output$phizero*exp(amak_std$value[which(amak_std$name=="log_Rzero")])*(1-om_input$h)/(5*om_input$h-1)

phiF1 <- (amak_output$SSB[which(amak_output$SSB[,1]==1),2]+beta)/alpha
Req1 <- amak_output$SSB[which(amak_output$SSB[,1]==1),2]/((amak_output$SSB[which(amak_output$SSB[,1]==1),2]+beta)/alpha)

phiF2 <- (amak_std$value[which(amak_std$name=="Bmsy")]+beta)/alpha
Req2 <- amak_std$value[which(amak_std$name=="Bmsy")]/((amak_std$value[which(amak_std$name=="Bmsy")]+beta)/alpha)

parameter_values[,2] <- c(exp(amak_std$value[which(amak_std$name=="log_Rzero")]),
                          amak_output$phizero,
                          phiF1, 
                          Req1,
                          phiF2,
                          Req2)

meanr <- exp(amak_std$value[which(amak_std$name=="mean_log_rec")])
amak_resid <- exp(amak_std$value[which(amak_std$name=="rec_dev")])[13:length(amak_std$value[which(amak_std$name=="rec_dev")])]
amak_predr <- amak_output$N[,2] #amak_output$Stock_Rec[,4] (meanr*amak_resid)
amak_ssb <- amak_output$SSB[which(amak_output$SSB[,1]>=om_input$yr[1] &  amak_output$SSB[,1]<= om_input$yr[length(om_input$yr)]),2]
#amak_expr <- BH.fcn(S=amak_ssb, h=om_input$h, R0=exp(amak_std$value[which(amak_std$name=="log_Rzero")]), Phi.0 = amak_output$phizero)
amak_expr <- amak_output$Stock_Rec[which(amak_output$Stock_Rec[,1]>1),3]

write.csv(amak_ssb, file="C:\\Users\\bai.li\\Desktop\\ssb.csv")
write.csv(amak_expr, file="C:\\Users\\bai.li\\Desktop\\exp_r.csv")
write.csv(amak_predr, file="C:\\Users\\bai.li\\Desktop\\pred_r.csv")
write.csv(amak_resid, file="C:\\Users\\bai.li\\Desktop\\exp_resid.csv")

#### ASAP ####
subdir = "ASAP"
asap_output <- dget(file.path(maindir, "output", subdir, paste("s", i, sep=""), "asap3.rdat"))
setwd(file.path(maindir, "output", subdir, paste("s", i, sep="")))
asap_std <- readRep("asap3", suffix = ".std")

#asap_msy=msy_calcs(steep=om_input$h, R0=asap_output$SR.parms$SR.R0*1000, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=asap_output$fleet.sel.mats$sel.m.fleet1[1,], selD=rep(0,om_input$nages), selZ=asap_output$fleet.sel.mats$sel.m.fleet1[1,], mat.f=om_input$mat.age, mat.m=NULL, BC=1, maxF=1.0, step=0.00001)
asap_msy=msy_calcs(steep=om_input$h, R0=asap_output$SR.parms$SR.R0*1000, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=asap_output$fleet.sel.mats$sel.m.fleet1[1,], selD=rep(0,om_input$nages), selZ=asap_output$fleet.sel.mats$sel.m.fleet1[1,], mat.f=om_input$mat.age, mat.m=NULL, BC=exp(0.6^2/2), maxF=1.0, step=0.00001)

asap_msy$R_eq[1]
asap_msy$msy; asap_msy$Fmsy; asap_msy$SSBmsy
asap_std$value[which(asap_std$name=="MSY")]; asap_std$value[which(asap_std$name=="Fmsy_report")]; asap_std$value[which(asap_std$name=="SSBmsy_report")]


alpha=asap_output$SR.parms$SR.alpha*1000 #4*asap_output$SR.parms$SR.R0*1000*om_input$h/(5*om_input$h-1)
beta=asap_output$SR.parms$SR.beta #asap_output$SR.parms$SR.SPR0/1000*asap_output$SR.parms$SR.R0*1000*(1-om_input$h)/(5*om_input$h-1)

phiF1 <- (asap_output$SSB[1]+beta)/alpha
Req1 <- asap_output$SSB[1]/((asap_output$SSB[1]+beta)/alpha)

phiF2 <- (asap_std$value[which(asap_std$name=="SSBmsy_report")]+beta)/alpha
Req2 <- asap_std$value[which(asap_std$name=="SSBmsy_report")]/((asap_std$value[which(asap_std$name=="SSBmsy_report")]+beta)/alpha)

parameter_values[,3] <- c(asap_output$SR.parms$SR.R0*1000,
                          asap_output$SR.parms$SR.SPR0/1000,
                          phiF1, 
                          Req1,
                          phiF2,
                          Req2)

asap_expr <- BH.fcn(S=asap_output$SSB, h=asap_output$SR.parms$SR.steepness, R0=asap_output$SR.parms$SR.R0, Phi.0 = asap_output$SR.parms$SR.SPR0)*1000
asap_output$SR.resids
asap_output$SR.resids$R.no.devs
asap_output$SR.resids$R.no.devs*exp(asap_output$SR.resids$logR.dev)
asap_output$SR.resids$recruits
asap_output$N.age[,1]

write.csv(asap_output$SSB, file="C:\\Users\\bai.li\\Desktop\\ssb.csv")
write.csv(asap_output$SR.resids$R.no.devs*1000, file="C:\\Users\\bai.li\\Desktop\\exp_r.csv") #2:30
write.csv(exp(asap_output$SR.resids$logR.dev), file="C:\\Users\\bai.li\\Desktop\\exp_resid.csv")
write.csv(asap_output$SR.resids$recruits*1000, file="C:\\Users\\bai.li\\Desktop\\pred_r.csv")

#### BAM ####
subdir = "BAM"
bam_output <- dget(file.path(maindir, "output", subdir, paste("s", i, sep=""), "bam-sim.rdat"))

#bam_msy=msy_calcs(steep=om_input$h, R0=bam_output$parms$R.virgin.bc, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=bam_output$sel.age$sel.v.wgted.L, selD=rep(0,om_input$nages), selZ=bam_output$sel.age$sel.v.wgted.L, mat.f=om_input$mat.age, mat.m=NULL, BC=exp(0.6^2/2), maxF=1.0, step=0.0001)
bam_msy=msy_calcs(steep=om_input$h, R0=bam_output$parms$BH.R0, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=bam_output$sel.age$sel.v.wgted.L, selD=rep(0,om_input$nages), selZ=bam_output$sel.age$sel.v.wgted.L, mat.f=om_input$mat.age, mat.m=NULL, BC=exp(0.6^2/2), maxF=1.0, step=0.01)
bam_msy$R_eq[1]
bam_msy$msy; bam_msy$Fmsy; bam_msy$SSBmsy
bam_output$parms$msy.mt; bam_output$parms$Fmsy; bam_output$parms$SSBmsy

alpha=4*bam_output$parms$BH.R0*bam_output$parms$BH.steep/(5*bam_output$parms$BH.steep-1)
beta=bam_output$parms$BH.Phi0*bam_output$parms$BH.R0*(1-bam_output$parms$BH.steep)/(5*bam_output$parms$BH.steep-1)

phiF1 <- (bam_output$t.series$SSB[1]+beta)/alpha/exp(0.6^2/2)
Req1 <- bam_output$t.series$SSB[1]/((bam_output$t.series$SSB[1]+beta)/alpha/exp(0.6^2/2))

phiF2 <- (bam_output$parms$SSBmsy+beta)/alpha/exp(0.6^2/2)
Req2 <- bam_output$parms$SSBmsy/((bam_output$parms$SSBmsy+beta)/alpha/exp(0.6^2/2))

parameter_values[,4] <- c(bam_output$parms$BH.R0,
                          bam_output$parms$BH.Phi0,
                          phiF1, 
                          Req1,
                          phiF2,
                          Req2)

#bam_output$parms$R.virgin.bc
#bam_output$eq.series

bam_ssb <- bam_output$t.series$SSB[1:om_input$yr[length(om_input$yr)]]
bam_expr <- BH.fcn(S=bam_ssb, h=bam_output$parms$BH.steep, R0=bam_output$parms$BH.R0, Phi.0 = bam_output$parms$BH.Phi0)
#bam_expr <- BH.fcn(S=bam_ssb, h=bam_output$parms$BH.steep, R0=bam_output$parms$R.virgin.bc, Phi.0 = bam_output$parms$BH.Phi0)
bam_resid <- exp(bam_output$parm.tvec$log.rec.dev)
bam_expr[1:29]*bam_resid[2:30]
bam_output$t.series$recruits

write.csv(bam_ssb, file="C:\\Users\\bai.li\\Desktop\\ssb.csv")
write.csv(bam_expr, file="C:\\Users\\bai.li\\Desktop\\exp_r.csv")
write.csv(bam_output$t.series$recruits, file="C:\\Users\\bai.li\\Desktop\\pred_r.csv")
write.csv(exp(bam_output$parm.tvec$log.rec.dev), file="C:\\Users\\bai.li\\Desktop\\exp_resid.csv")

#### SS ####
subdir = "SS"
ss_output <- SS_output(dir=file.path(maindir, "output", subdir, paste("s", i, sep="")), ncols = 300, verbose=F, printstats=F)
setwd(file.path(maindir, "output", subdir, paste("s", i, sep="")))
ss_std <- readRep("ss", suffix = ".std")
msy_std <- ss_std[ss_std$name=="Mgmt_quant",]

ss_msy=msy_calcs(steep=om_input$h, R0=exp(ss_output$estimated_non_dev_parameters$Value[which(rownames(ss_output$estimated_non_dev_parameters)=="SR_LN(R0)")])*1000, M=matrix(rep(om_input$M.age, times=om_input$nyr), nrow=om_input$nyr, byrow=T), wgt=om_input$W.mt, prop.f=om_input$proportion.female, selL=as.numeric(ss_output$ageselex[which(ss_output$ageselex$Label=="1_1Asel"),9:20]), selD=rep(0,om_input$nages), selZ=as.numeric(ss_output$ageselex[which(ss_output$ageselex$Label=="1_1Asel"),9:20]), mat.f=om_input$mat.age, mat.m=NULL, BC=1, maxF=1.0, step=0.0001)

ss_msy$R_eq[1]
ss_msy$msy; ss_msy$Fmsy; ss_msy$SSBmsy
msy_std[15, "value"]; msy_std[14, "value"]; msy_std[12, "value"]

ss_rzero <- exp(ss_output$estimated_non_dev_parameters$Value[which(rownames(ss_output$estimated_non_dev_parameters)=="SR_LN(R0)")])*1000
ss_phizero <- (ss_output$sprseries$SSBzero/ss_rzero)[1]

alpha=4*ss_rzero*om_input$h/(5*om_input$h-1)
beta=ss_phizero*ss_rzero*(1-om_input$h)/(5*om_input$h-1)

phiF1 <- (ss_output$timeseries$SpawnBio[which(ss_output$timeseries$Yr==1)]+beta)/alpha
Req1 <- ss_output$timeseries$SpawnBio[which(ss_output$timeseries$Yr==1)]/((ss_output$timeseries$SpawnBio[which(ss_output$timeseries$Yr==1)]+beta)/alpha)

phiF2 <- (msy_std[12, "value"]+beta)/alpha
Req2 <- msy_std[12, "value"]/((msy_std[12, "value"]+beta)/alpha)

parameter_values[,5] <- c(ss_rzero,
                          ss_phizero,
                          phiF1, 
                          Req1,
                          phiF2,
                          Req2)

ss_ssb <- ss_output$recruit$SpawnBio[which(ss_output$recruit$Yr>=1 & ss_output$recruit$Yr<=30)]
ss_expr <- ss_output$recruit$exp_recr[which(ss_output$recruit$Yr>=1 & ss_output$recruit$Yr<=30)]*1000 #BH.fcn(S=ss_ssb, h=om_input$h, R0=ss_rzero, Phi.0 = (ss_output$sprseries$SSBzero/ss_rzero)[1])
ss_radj <- ss_output$recruit$bias_adjusted[which(ss_output$recruit$Yr>=1 & ss_output$recruit$Yr<=30)]*1000 #ss_expr*exp(-0.6^2/2)
ss_resid <- exp(ss_output$recruit$dev[which(ss_output$recruit$Yr>=1 & ss_output$recruit$Yr<=30)])
#ss_predr_nobiascor <- ss_expr*ss_resid 
ss_predr <- ss_output$recruit$pred_recr[which(ss_output$recruit$Yr>=1 & ss_output$recruit$Yr<=30)]*1000 #ss_radj*ss_resid

write.csv(ss_ssb, file="C:\\Users\\bai.li\\Desktop\\ssb.csv")
write.csv(ss_expr, file="C:\\Users\\bai.li\\Desktop\\exp_r.csv")
write.csv(ss_radj, file="C:\\Users\\bai.li\\Desktop\\pred_radj.csv")
write.csv(ss_predr , file="C:\\Users\\bai.li\\Desktop\\pred_r.csv")
write.csv(ss_resid, file="C:\\Users\\bai.li\\Desktop\\exp_resid.csv")

jpeg(file=file.path(maindir, "figure", "Rexp.jpg"), width=170, height=150, units="mm", res=300)
plot(2:30, om_rexp[1:29], pch=19, xlab="Year", ylab="R", ylim=c(500000, 1250000))
points(2:30,om_rexp[1:29]*exp(-0.6^2/2), pch=1)
lines(2:30, amak_expr[1:29], col="orange")
lines(2:30, amak_expr[1:29]*exp(0.6^2/2), lty=2, col="orange")
lines(2:30, asap_expr[1:29], col="green")
lines(2:30, asap_expr[1:29]*exp(0.6^2/2), lty=2, col="green")
lines(2:30, bam_expr[1:29], col="red")
lines(2:30, bam_expr[1:29]*exp(0.6^2/2), col="red", lty=2)
lines(2:30, ss_expr[1:29]*exp(-0.6^2/2), col="deepskyblue3")
lines(2:30, ss_expr[1:29], lty=2, col="deepskyblue3")

legend("bottomleft", 
       c("OM_Rexp", "AMAK_Rexp", "ASAP_Rexp", "BAM_Rexp", "SS_Rexp*exp(-BC)",
         "OM_Rexp*exp(-BC)", "AMAK_Rexp*exp(BC)", "ASAP_Rexp*exp(BC)", "BAM_Rexp*exp(BC)", "SS_Rexp"),
       col=c("black", "orange", "green", "red",  "deepskyblue3", 
             "black",  "orange", "green", "red", "deepskyblue3"),
       lty=c(NA, 1, 1, 1, 1,
             NA,2, 2, 2, 2),
       pch=c(19, NA, NA, NA, NA,
             1, NA, NA, NA, NA),
       bty="n",
       cex=0.7, 
       title="Approach B",
       ncol=2)

dev.off()



jpeg(file=file.path(maindir, "figure", "Rexp.jpg"), width=170, height=150, units="mm", res=300)

plot(2:30, om_rexp[1:29], pch=19, xlab="Year", ylab="R", ylim=c(500000, 1250000))
lines(2:30, amak_expr[1:29], col="orange")
lines(2:30, asap_expr[1:29], col="green")
lines(2:30, bam_expr[1:29], col="red")
lines(2:30, bam_expr[1:29]*exp(0.6^2/2), col="red", lty=2)
lines(2:30, ss_expr[1:29], col="deepskyblue3")
legend("topright", 
       c("OM_Rexp","AMAK_Rexp", "ASAP_Rexp", "BAM_Rexp", "BAM_Rexp*exp(BC)", "SS_Rexp"), 
       col=c("black", "orange", "green", "red", "red", "deepskyblue3"),
       lty=c(NA,1,1,1,2,1),
       pch=c(19, NA, NA, NA, NA, NA),
       bty="n",
       cex=0.7, 
       title="Approach B")

dev.off()

jpeg(file=file.path(maindir, "figure", "Rpred.jpg"), width=170, height=150, units="mm", res=300)

plot(1:30, om_output$N.age[,1], pch=19, xlab="Year", ylab="R", ylim=c(200000, 3000000))
lines(1:30, amak_predr, col="orange")
lines(1:30, asap_output$N.age[,1]*1000, col="green")
lines(1:30, bam_output$t.series$recruits[1:30], col="red")
lines(1:30, ss_output$natage_annual_2_with_fishery[which(ss_output$natage_annual_2_with_fishery$Yr>=om_input$yr[1] & ss_output$natage_annual_2_with_fishery$Yr<=om_input$yr[length(om_input$yr)]),5]*1000, col="deepskyblue3")
legend("topright", 
       c("OM_Rpred","AMAK_Rpred", "ASAP_Rpred", "BAM_Rpred", "SS_Rpred"), 
       col=c("black", "orange", "green", "red", "deepskyblue3"),
       lty=c(NA,1,1,1,1),
       pch=c(19, NA, NA, NA, NA),
       bty="n",
       cex=0.7, 
       title="Approach B")

dev.off()

jpeg(file=file.path(maindir, "figure", "YF.jpg"), width=170, height=150, units="mm", res=300)
library(scales)
plot(om_output$msy$f_seq, om_output$msy$L_eq, col="gray", pch=19, xlab="F", ylab="Yield", ylim=c(0, 1500), xlim=c(0,1))
abline(v=om_output$msy$Fmsy, lty=2, col=alpha("black", 0.5))
abline(h=om_output$msy$msy, lty=2, col=alpha("black", 0.5))
lines(amak_msy$f_seq, amak_msy$L_eq, col="orange")
abline(v=amak_msy$Fmsy, lty=2, col=alpha("orange", 0.5))
abline(h=amak_msy$msy, lty=2, col=alpha("orange", 0.5))
lines(asap_msy$f_seq, asap_msy$L_eq, col="green")
abline(v=asap_msy$Fmsy, lty=2, col=alpha("green", 0.5))
abline(h=asap_msy$msy, lty=2, col=alpha("green", 0.5))
lines(bam_output$eq.series$F.eq, bam_output$eq.series$L.eq.mt, col="red")
abline(v=bam_output$parms$Fmsy, lty=2, col=alpha("red", 0.5))
abline(h=bam_output$parms$msy.mt, lty=2, col=alpha("red", 0.5))
lines(ss_msy$f_seq,ss_msy$L_eq, col="deepskyblue3")
abline(v=ss_msy$Fmsy, lty=2, col=alpha("deepskyblue3", 0.5))
abline(h=ss_msy$msy, lty=2, col=alpha("deepskyblue3", 0.5))
#lines(ss_output$equil_yield$Fmult, ss_output$equil_yield$Tot_Catch, col="deepskyblue3", lty=2)
abline(v=om_output$msy$Fmsy, lty=2, col=alpha("black", 0.5))
legend("right", 
       c("OM","AMAK", "ASAP", "BAM", "SS"), 
       col=c("gray", "orange", "green", "red", "deepskyblue3"),
       lty=c(NA,1,1,1,1),
       pch=c(19, NA, NA, NA, NA),
       bty="n",
       cex=0.7, 
       title="Approach B")

dev.off()

jpeg(file=file.path(maindir, "figure", "YF_ad_hoc.jpg"), width=170, height=150, units="mm", res=300)
library(scales)
plot(om_output$msy$f_seq, om_output$msy$L_eq, col="gray", pch=19, xlab="F", ylab="Yield", ylim=c(0, 1500), xlim=c(0,1))
abline(v=om_output$msy$Fmsy, lty=2, col=alpha("black", 0.5))
abline(h=om_output$msy$msy, lty=2, col=alpha("black", 0.5))
lines(amak_msy$f_seq, amak_msy$L_eq, col="orange")
abline(v=amak_msy$Fmsy, lty=2, col=alpha("orange", 0.5))
abline(h=amak_msy$msy, lty=2, col=alpha("orange", 0.5))
lines(asap_msy$f_seq, asap_msy$L_eq, col="green")
abline(v=asap_msy$Fmsy, lty=2, col=alpha("green", 0.5))
abline(h=asap_msy$msy, lty=2, col=alpha("green", 0.5))
lines(bam_output$eq.series$F.eq, bam_output$eq.series$L.eq.mt, col="red")
abline(v=bam_output$parms$Fmsy, lty=2, col=alpha("red", 0.5))
abline(h=bam_output$parms$msy.mt, lty=2, col=alpha("red", 0.5))
lines(ss_msy$f_seq,ss_msy$L_eq, col="deepskyblue3")
abline(v=ss_msy$Fmsy, lty=2, col=alpha("deepskyblue3", 0.5))
abline(h=ss_msy$msy, lty=2, col=alpha("deepskyblue3", 0.5))
#lines(ss_output$equil_yield$Fmult, ss_output$equil_yield$Tot_Catch, col="deepskyblue3", lty=2)
abline(v=om_output$msy$Fmsy, lty=2, col=alpha("black", 0.5))
legend("right", 
       c("OM","AMAK_Ad_hoc", "ASAP_Ad_hoc", "BAM", "SS"), 
       col=c("gray", "orange", "green", "red", "deepskyblue3"),
       lty=c(NA,1,1,1,1),
       pch=c(19, NA, NA, NA, NA),
       bty="n",
       cex=0.7, 
       title="Approach B")

dev.off()


