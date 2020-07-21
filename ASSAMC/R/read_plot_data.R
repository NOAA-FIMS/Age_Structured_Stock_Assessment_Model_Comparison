## Aggregate data of biomass, abundance, SSB, recruitment, F (apical F*selectivity), F multiplier, landings, and survey from models to matrix
read_plot_data <- function(em_names){
  #library(PBSadmb)
  #library(r4ss)
  library(jsonlite)

  ## OM
  subdir = "OM"
  load(file.path(maindir, "output", subdir, paste("OM", 1, ".RData", sep="")))
  om_biomass <- om_abundance <- om_ssb <- om_recruit <- om_Ftot <- om_Fmul <- om_landing <- om_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
  om_msy <- om_fmsy <- om_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
  om_fratio <- om_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
  om_agecomp <- list()
  om_landing_err <- om_survey_err <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)

  for (om_sim in 1:keep_sim_num){
    load(file.path(maindir, "output", subdir, paste("OM", keep_sim_id[om_sim], ".RData", sep="")))
    om_biomass[,om_sim] <- om_output$biomass.mt
    om_abundance[,om_sim] <- om_output$abundance/1000
    om_ssb[,om_sim] <- om_output$SSB
    om_recruit[,om_sim] <- om_output$N.age[,1]/1000
    om_Ftot[,om_sim] <- apply(om_output$FAA, 1, max)
    om_Fmul[,om_sim] <- om_output$f
    om_landing[,om_sim] <- om_output$L.mt$fleet1
    om_survey[,om_sim] <- om_output$survey_index$survey1
    om_msy[, om_sim] <- om_output$msy$msy
    om_fmsy[, om_sim] <- round(om_output$msy$Fmsy, digits = 3)
    om_ssbmsy[, om_sim] <- om_output$msy$SSBmsy
    om_fratio[, om_sim] <- om_Ftot[, om_sim]/om_fmsy[om_sim]
    om_ssbratio[, om_sim] <- om_ssb[, om_sim]/om_ssbmsy[om_sim]
    om_agecomp[[om_sim]] <- apply(om_output$N.age/1000, 1, function(x) x/sum(x))
    #om_landing_err[,om_sim] <- em_input$L.obs$fleet1
    #om_survey_err[,om_sim] <- em_input$survey.obs$survey1
  }
  om_list <- list(om_biomass, om_abundance, om_ssb, om_recruit, om_Ftot, om_landing, om_survey, om_msy, om_fmsy, om_ssbmsy, om_fratio, om_ssbratio, om_agecomp)
  names(om_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
  om_list <<- om_list
  save(om_list, file=file.path(maindir, "output", "om_output.RData"))

  ## AMAK
  if ("AMAK" %in% em_names){
    amak_biomass <- amak_abundance <- amak_ssb <- amak_recruit <- amak_Ftot <- amak_Fmul <- amak_landing <- amak_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    amak_msy <- amak_fmsy <- amak_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
    amak_fratio <- amak_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    amak_agecomp <- list()

    subdir = "AMAK"
    for (om_sim in 1:keep_sim_num){
      setwd(file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep="")))
      amak_output <- readRep("For_R", suffix = ".rep")
      amak_std <- readRep("amak", suffix = ".std")
      amak_biomass[,om_sim] <- amak_output$TotBiom[which(amak_output$TotBiom[,1]>=om_input$year[1] & amak_output$TotBiom[,1]<=om_input$year[length(om_input$year)]),2]
      amak_abundance[,om_sim] <- apply(amak_output$N[,2:ncol(amak_output$N)], 1, sum)/1000
      amak_ssb[,om_sim] <- amak_output$SSB[which(amak_output$SSB[,1]>=om_input$year[1] &  amak_output$SSB[,1]<= om_input$year[length(om_input$year)]),2]
      amak_recruit[,om_sim] <- amak_output$R[,2]/1000
      amak_Ftot[,om_sim] <- apply(amak_output$TotF, 1, max)
      amak_Fmul[,om_sim] <- NA
      amak_landing[,om_sim] <- amak_output$Pred_catch_1
      amak_survey[,om_sim] <- amak_output$Obs_Survey_1[,3]
      amak_msy[, om_sim] <- amak_std$value[which(amak_std$name=="MSY")]
      amak_fmsy[, om_sim] <- round(amak_std$value[which(amak_std$name=="Fmsy")], digits = 3)
      amak_ssbmsy[, om_sim] <- amak_std$value[which(amak_std$name=="Bmsy")]
      amak_fratio[, om_sim] <- amak_Ftot[, om_sim]/amak_fmsy[om_sim]
      amak_ssbratio[, om_sim] <- amak_ssb[,om_sim]/amak_ssbmsy[om_sim]
      amak_agecomp[[om_sim]] <- apply(amak_output$N[,2:ncol(amak_output$N)]/1000, 1, function(x) x/sum(x))
    }
    amak_list <- list(amak_biomass, amak_abundance, amak_ssb, amak_recruit, amak_Ftot, amak_landing, amak_survey, amak_msy, amak_fmsy, amak_ssbmsy, amak_fratio, amak_ssbratio, amak_agecomp)
    names(amak_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
    amak_list <<- amak_list
    save(amak_list, file=file.path(maindir, "output", "amak_output.RData"))
  }

  ## ASAP
  if ("ASAP" %in% em_names){
    asap_biomass <- asap_abundance <- asap_ssb <- asap_recruit <- asap_Ftot <- asap_Fmul <- asap_landing <- asap_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    asap_msy <- asap_fmsy <- asap_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
    asap_fratio <- asap_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    asap_agecomp <- list()

    subdir = "ASAP"
    for (om_sim in 1:keep_sim_num){
      asap_output <- dget(file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep=""), "asap3.rdat"))
      setwd(file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep="")))
      asap_std <- readRep("asap3", suffix = ".std")
      asap_biomass[,om_sim] <- asap_output$tot.jan1.B
      asap_abundance[,om_sim] <- apply(asap_output$N.age, 1, sum)
      asap_ssb[,om_sim] <- asap_output$SSB
      asap_recruit[,om_sim] <- asap_output$N.age[,1]
      asap_Ftot[,om_sim] <- apply(asap_output$fleet.FAA$FAA.directed.fleet1, 1, max)
      asap_Fmul[,om_sim] <- asap_output$fleet.Fmult
      asap_landing[,om_sim] <- asap_output$catch.pred
      asap_survey[,om_sim] <- asap_output$index.pred$ind01
      asap_msy[, om_sim] <- asap_std$value[which(asap_std$name=="MSY")]
      asap_fmsy[, om_sim] <- round(asap_std$value[which(asap_std$name=="Fmsy_report")], digits = 3)
      asap_ssbmsy[, om_sim] <- asap_std$value[which(asap_std$name=="SSBmsy_report")]
      asap_fratio[, om_sim] <- asap_Ftot[, om_sim]/asap_fmsy[om_sim]
      asap_ssbratio[, om_sim] <- asap_ssb[,om_sim]/asap_ssbmsy[om_sim]
      asap_agecomp[[om_sim]] <- apply(asap_output$N.age, 1, function(x) x/sum(x))
    }
    asap_list <- list(asap_biomass, asap_abundance, asap_ssb, asap_recruit, asap_Ftot, asap_landing, asap_survey, asap_msy, asap_fmsy, asap_ssbmsy, asap_fratio, asap_ssbratio, asap_agecomp)
    names(asap_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
    asap_list <<- asap_list
    save(asap_list, file=file.path(maindir, "output", "asap_output.RData"))
  }

  ## BAM
  if("BAM" %in% em_names){
    bam_biomass <- bam_abundance <- bam_ssb <- bam_recruit <- bam_Ftot <- bam_Fmul <- bam_landing <- bam_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    bam_msy <- bam_fmsy <- bam_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
    bam_fratio <- bam_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    bam_agecomp <- list()

    subdir = "BAM"
    for (om_sim in 1:keep_sim_num){
      bam_output <- dget(file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep=""), "bam-sim.rdat"))

      bam_biomass[,om_sim] <- bam_output$t.series$B[1:om_input$year[length(om_input$year)]]
      bam_abundance[,om_sim] <- bam_output$t.series$N[1:om_input$year[length(om_input$year)]]/1000
      bam_ssb[,om_sim] <- bam_output$t.series$SSB[1:om_input$year[length(om_input$year)]]
      bam_recruit[,om_sim] <- bam_output$t.series$recruits[1:om_input$year[length(om_input$year)]]/1000
      bam_Ftot[,om_sim] <- bam_output$t.series$F.full[1:om_input$year[length(om_input$year)]]
      bam_Fmul[,om_sim] <- NA
      bam_landing[,om_sim] <- bam_output$t.series$L.fleet1.pr[1:om_input$year[length(om_input$year)]]
      bam_survey[,om_sim] <- bam_output$t.series$U.survey1.pr[1:om_input$year[length(om_input$year)]]
      bam_msy[, om_sim] <- bam_output$parms$msy.mt
      bam_fmsy[, om_sim] <- round(bam_output$parms$Fmsy, digits = 3)
      bam_ssbmsy[, om_sim] <- bam_output$parms$SSBmsy
      bam_fratio[, om_sim] <- bam_output$t.series$F.Fmsy[1:om_input$year[length(om_input$year)]]
      bam_ssbratio[, om_sim] <- bam_output$t.series$SSB.SSBmsy[1:om_input$year[length(om_input$year)]]
      bam_agecomp[[om_sim]] <- apply(bam_output$N.age[1:om_input$nyr,]/1000, 1, function(x) x/sum(x))
    }
    bam_list <- list(bam_biomass, bam_abundance, bam_ssb, bam_recruit, bam_Ftot, bam_landing, bam_survey, bam_msy, bam_fmsy, bam_ssbmsy, bam_fratio, bam_ssbratio, bam_agecomp)
    names(bam_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
    bam_list <<- bam_list
    save(bam_list, file=file.path(maindir, "output", "bam_output.RData"))
  }

  ## SS
  if ("SS" %in% em_names){
    ss_biomass <- ss_abundance <- ss_ssb <- ss_recruit <- ss_Ftot <- ss_Fmul <- ss_landing <- ss_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    ss_msy <- ss_fmsy <- ss_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
    ss_fratio <- ss_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    ss_agecomp <- list()

    subdir = "SS"
    for (om_sim in 1:keep_sim_num){
      ss_output <- SS_output(dir=file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep="")), ncols = 300, verbose=F, printstats=F)

      setwd(file.path(maindir, "output", subdir, paste("s", keep_sim_id[om_sim], sep="")))
      ss_std <- readRep("ss", suffix = ".std")
      msy_std <- ss_std[ss_std$name=="Mgmt_quant",]

      ss_biomass[,om_sim] <- ss_output$timeseries$`SmryBio_SX:1_GP:1`[which(ss_output$timeseries$Yr>=om_input$year[1] & ss_output$timeseries$Yr<=om_input$year[length(om_input$year)])]
      ss_abundance[,om_sim] <- ss_output$timeseries$`SmryNum_SX:1_GP:1`[which(ss_output$timeseries$Yr>=om_input$year[1] & ss_output$timeseries$Yr<=om_input$year[length(om_input$year)])]
      ss_ssb[,om_sim] <- ss_output$timeseries$SpawnBio[which(ss_output$timeseries$Yr>=om_input$year[1] & ss_output$timeseries$Yr<=om_input$year[length(om_input$year)])]
      ss_recruit[,om_sim] <- ss_output$natage_annual_2_with_fishery[which(ss_output$natage_annual_2_with_fishery$Yr>=om_input$year[1] & ss_output$natage_annual_2_with_fishery$Yr<=om_input$year[length(om_input$year)]),5]
      ss_Ftot[,om_sim] <- ss_output$timeseries$`F:_1`[which(ss_output$timeseries$Yr>=om_input$year[1] & ss_output$timeseries$Yr<=om_input$year[length(om_input$year)])]
      ss_Fmul[,om_sim] <- NA
      ss_landing[,om_sim] <- ss_output$timeseries$`sel(B):_1`[which(ss_output$timeseries$Yr>=om_input$year[1] & ss_output$timeseries$Yr<=om_input$year[length(om_input$year)])]
      ss_survey[,om_sim] <- ss_output$cpue$Exp[which(ss_output$cpue$Fleet==2)]
      ss_msy[, om_sim] <- msy_std[15, "value"]
      ss_fmsy[, om_sim] <- round(msy_std[14, "value"], digits = 3)
      ss_ssbmsy[, om_sim] <- msy_std[12, "value"]
      ss_fratio[, om_sim] <- ss_output$Kobe$F.Fmsy[which(ss_output$Kobe$Yr>=om_input$year[1] & ss_output$Kobe$Yr<=om_input$year[length(om_input$year)])]
      ss_ssbratio[, om_sim] <- ss_output$Kobe$B.Bmsy[which(ss_output$Kobe$Yr>=om_input$year[1] & ss_output$Kobe$Yr<=om_input$year[length(om_input$year)])]
      ss_agecomp[[om_sim]] <- apply(ss_output$natage_annual_2_with_fishery[1:om_input$nyr, 5:ncol(ss_output$natage_annual_2_with_fishery)], 1, function(x) x/sum(x))
    }
    ss_list <- list(ss_biomass, ss_abundance, ss_ssb, ss_recruit, ss_Ftot, ss_landing, ss_survey, ss_msy, ss_fmsy, ss_ssbmsy, ss_fratio, ss_ssbratio, ss_agecomp)
    names(ss_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
    ss_list <<- ss_list
    save(ss_list, file=file.path(maindir, "output", "ss_output.RData"))
  }
  
  ## MAS
  if ("MAS" %in% em_names){
    mas_biomass <- mas_abundance <- mas_ssb <- mas_recruit <- mas_Ftot <- mas_Fmul <- mas_landing <- mas_survey <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    mas_msy <- mas_fmsy <- mas_ssbmsy <- matrix(NA, nrow=1, ncol=keep_sim_num)
    mas_fratio <- mas_ssbratio <- matrix(NA, nrow=om_input$nyr, ncol=keep_sim_num)
    mas_agecomp <- list()
    
    subdir = "MAS"
    for (om_sim in 1:keep_sim_num){
      mas_output <- read_json(file.path(maindir, "output",  subdir, paste("s", keep_sim_id[om_sim], sep=""), paste("s", keep_sim_id[om_sim], ".json", sep="")))
      popdy<-mas_output$population_dynamics
      pop<-popdy$populations[[1]]
      flt<-popdy$fleets[[1]]
      srvy<-popdy$surveys[[1]]
      
      mas_biomass[,om_sim] <- unlist(pop$undifferentiated$biomass$values)
      mas_abundance[,om_sim] <- unlist(pop$undifferentiated$abundance$values)
      mas_ssb[,om_sim] <- unlist(pop$undifferentiated$spawning_stock_biomass$values)
      mas_recruit[,om_sim] <- unlist(pop$undifferentiated$recruits$values)
      mas_Ftot[,om_sim] <- unlist(pop$undifferentiated$fishing_mortality$values)
      mas_Fmul[,om_sim] <- NA
      mas_landing[,om_sim] <- unlist(flt$undifferentiated$catch_biomass$values)
      mas_survey[,om_sim] <- unlist(srvy$undifferentiated$survey_biomass$values)
      mas_msy[, om_sim] <- pop$MSY$B_msy
      mas_fmsy[, om_sim] <- round(pop$MSY$F_msy, digits = 3)
      #mas_fmsy[, om_sim] <- round(pop$females$MSY$F_msy, digits = 3)
      mas_ssbmsy[, om_sim] <- pop$females$MSY$SSB_msy
      mas_fratio[, om_sim] <- mas_Ftot[, om_sim]/mas_fmsy[om_sim]
      mas_ssbratio[, om_sim] <- mas_ssb[,om_sim]/mas_ssbmsy[om_sim]
      mas_agecomp[[om_sim]] <- apply(matrix(unlist(pop$undifferentiated$numbers_at_age$values), nrow=popdy$nyears, ncol=popdy$nages, byrow = T), 1, function(x) x/sum(x))
    }
    mas_list <- list(mas_biomass, mas_abundance, mas_ssb, mas_recruit, mas_Ftot, mas_landing, mas_survey, mas_msy, mas_fmsy, mas_ssbmsy, mas_fratio, mas_ssbratio, mas_agecomp)
    names(mas_list) <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio", "agecomp")
    mas_list <<- mas_list
    save(mas_list, file=file.path(maindir, "output", "mas_output.RData"))
  }
}

