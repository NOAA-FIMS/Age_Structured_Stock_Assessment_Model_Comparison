#' @export
run_om <- function(input_list=NULL,
                   show_iter_num=FALSE){
  maindir <- input_list$maindir
  subdir <- "OM"
  case_name <- input_list$case_name

  ## Error checks for missing files
  if (is.null(maindir)) stop ("Missing main working directory!")

  ## Set up required working folders (output and figure)
  if (!file.exists(file.path(maindir, case_name))) dir.create(file.path(maindir, case_name))
  casedir <- file.path(maindir, case_name)
  invisible(sapply(c("output", "figure"), function(x) {
    if (!file.exists(file.path(casedir, x))) dir.create(file.path(casedir, x))
  }))

  invisible(sapply(c("OM"), function(x) {
    if (!file.exists(file.path(casedir, "output", x))) dir.create(file.path(casedir, "output", x))
  }))

  ## Remove existing simulated OM data
  invisible(do.call(file.remove, list(list.files(file.path(casedir, "output", "OM"), full.names = TRUE))))

  ## Specify seeds
  if (is.null(input_list$seed_num)) {
    set.seed(9924)
  } else {
    set.seed(input_list$seed_num)
  }

  om_stocks <- vector(mode="list", length=length(input_list$stocks))
  names(om_stocks) <- paste("stock", 1:length(om_stocks), sep="")

  for (i in 1:length(input_list$stocks)){
    if (input_list$stocks[[i]]$initial_equilibrium_F==FALSE){
      input_list$stocks[[i]]$year <- input_list$stocks[[i]]$year[1]:(input_list$stocks[[i]]$year[length(input_list$stocks[[i]]$year)]+1)
    }

    nyr <<- length(input_list$stocks[[i]]$year)
    nages <<- length(input_list$stocks[[i]]$ages)

    ## Set up F deviations-at-age per iteration
    om_stocks[[i]]$f_dev_matrix <- f_dev_case(
      f_dev_change=input_list$stocks[[i]]$f_dev_change,
      nyr=nyr,
      logf_sd=input_list$stocks[[i]]$logf_sd,
      om_sim_num=input_list$om_sim_num)


    om_stocks[[i]]$f_matrix <- f_case(
      f_pattern=input_list$stocks[[i]]$f_pattern,
      start_val=input_list$stocks[[i]]$start_val,
      middle_val=input_list$stocks[[i]]$middle_val,
      end_val=input_list$stocks[[i]]$end_val,
      f_val=input_list$stocks[[i]]$f_val,
      start_year=input_list$stocks[[i]]$start_year,
      middle_year=input_list$stocks[[i]]$middle_year,
      nyr=nyr,
      om_sim_num=input_list$om_sim_num,
      f_dev_matrix=om_stocks[[i]]$f_dev_matrix,
      initial_equilibrium_F=input_list$stocks[[i]]$initial_equilibrium_F)

    ## Set up R deviations-at-age per iteration
    om_stocks[[i]]$r_dev_matrix <- r_dev_case(
      r_dev_change=input_list$stocks[[i]]$r_dev_change,
      nyr=nyr,
      logR_sd=input_list$stocks[[i]]$logR_sd,
      om_sim_num=input_list$om_sim_num)

    ## Create and fill vectors to be used in the population model
    om_stocks[[i]]$len=input_list$stocks[[i]]$Linf*(1-exp(-input_list$stocks[[i]]$K*(input_list$stocks[[i]]$ages-input_list$stocks[[i]]$a0))) #von Bertalanffy growth
    om_stocks[[i]]$W.kg=input_list$stocks[[i]]$a.lw *om_stocks[[i]]$len^input_list$stocks[[i]]$b.lw  #Weight-length relationship, assumed to be in kg
    om_stocks[[i]]$W.mt=om_stocks[[i]]$W.kg/1000 #Weight in mt
    om_stocks[[i]]$M.age=rep(input_list$stocks[[i]]$M, nages) #natural mortality at age
    om_stocks[[i]]$mat.age=logistic(
      pattern=input_list$stocks[[i]]$pattern.mat,
      x=input_list$stocks[[i]]$ages,
      a1=input_list$stocks[[i]]$slope.mat,
      b1=input_list$stocks[[i]]$A50.mat) #maturity at age
    om_stocks[[i]]$proportion.female=rep(input_list$stocks[[i]]$female.proportion, nages) #proportion female at age
  }

  setwd(paste(maindir))

  om_input <- vector(mode="list", length=length(input_list$stocks))
  names(om_input) <- paste("stock", 1:length(om_input), sep="")

  for(om_sim in 1:input_list$om_sim_num){

    for (i in 1:length(input_list$stocks)){
      if (om_sim>1 & input_list$stocks[[i]]$initial_equilibrium_F==FALSE){
        input_list$stocks[[i]]$year <- input_list$stocks[[i]]$year[1]:(input_list$stocks[[i]]$year[length(input_list$stocks[[i]]$year)]+1)
        nyr <- length(input_list$stocks[[i]]$year)
      }

      om_stocks[[i]]$logR.resid <- om_stocks[[i]]$r_dev_matrix[om_sim,]
      om_stocks[[i]]$logf.resid <- om_stocks[[i]]$f_dev_matrix[om_sim,]
      om_stocks[[i]]$f <- om_stocks[[i]]$f_matrix[om_sim,]

      ## Selectivity for fleets
      om_stocks[[i]]$selex_fleet <- sapply(
        1:input_list$stocks[[i]]$fleet_num,
        function(x)
          logistic(pattern=input_list$stocks[[i]]$sel_fleet[[x]]$pattern,
                   x=input_list$stocks[[i]]$ages,
                   a1=input_list$stocks[[i]]$sel_fleet[[x]]$slope.sel1,
                   b1=input_list$stocks[[i]]$sel_fleet[[x]]$A50.sel1,
                   a2=input_list$stocks[[i]]$sel_fleet[[x]]$slope.sel2,
                   b2=input_list$stocks[[i]]$sel_fleet[[x]]$A50.sel2))

      om_stocks[[i]]$selex_fleet <- lapply(seq_len(ncol(om_stocks[[i]]$selex_fleet)), function(x) om_stocks[[i]]$selex_fleet[,x])
      names(om_stocks[[i]]$selex_fleet) <- paste("fleet", 1:input_list$stocks[[i]]$fleet_num, sep="")

      ## Selectivity for surveys
      om_stocks[[i]]$selex_survey <- sapply(
        1:input_list$stocks[[i]]$survey_num,
        function(x)
          logistic(pattern=input_list$stocks[[i]]$sel_survey[[x]]$pattern,
                   x=input_list$stocks[[i]]$ages,
                   a1=input_list$stocks[[i]]$sel_survey[[x]]$slope.sel1,
                   b1=input_list$stocks[[i]]$sel_survey[[x]]$A50.sel1,
                   a2=input_list$stocks[[i]]$sel_survey[[x]]$slope.sel2,
                   b2=input_list$stocks[[i]]$sel_survey[[x]]$A50.sel2))

      om_stocks[[i]]$selex_survey <- lapply(seq_len(ncol(om_stocks[[i]]$selex_survey)), function(x) om_stocks[[i]]$selex_survey[,x])
      names(om_stocks[[i]]$selex_survey) <- paste("survey", 1:input_list$stocks[[i]]$survey_num, sep="")

      ## Compute the number of spawners per recruit of an unfished population (Phi.0)
      om_stocks[[i]]$N.pr0=rep(1,nages) #Number of spawners per recruit at age
      for (a in 1:(nages-1)) {
        om_stocks[[i]]$N.pr0[a+1]=om_stocks[[i]]$N.pr0[a]*exp(-om_stocks[[i]]$M.age[a])
      }
      om_stocks[[i]]$N.pr0[nages]=om_stocks[[i]]$N.pr0[nages]/(1-exp(-om_stocks[[i]]$M.age[nages]))  #Plus group
      om_stocks[[i]]$Phi.0=sum(om_stocks[[i]]$N.pr0*om_stocks[[i]]$proportion.female*om_stocks[[i]]$mat.age*om_stocks[[i]]$W.mt)     #Spawners per recruit based on mature female biomass

      ## Convert mean R0 and mean h to median R0 and median h
      if(is.null(input_list$stocks[[i]]$median_R0) | is.null(input_list$stocks[[i]]$median_h)) {
        input_list$stocks[[i]]$median_R0 <- convertSRparms(
          R0=input_list$stocks[[i]]$mean_R0,
          h=input_list$stocks[[i]]$mean_h,
          phi=om_stocks[[i]]$Phi.0,
          sigmaR=input_list$stocks[[i]]$logR_sd,
          mean2med=TRUE,
          model=input_list$stocks[[i]]$SRmodel)$R0BC

        input_list$stocks[[i]]$median_h <- convertSRparms(
          R0=input_list$stocks[[i]]$mean_R0,
          h=input_list$stocks[[i]]$mean_h,
          phi=om_stocks[[i]]$Phi.0,
          sigmaR=input_list$stocks[[i]]$logR_sd,
          mean2med=TRUE,
          model=input_list$stocks[[i]]$SRmodel)$hBC
      } else {
        input_list$stocks[[i]]$mean_R0 <- convertSRparms(
          R0=input_list$stocks[[i]]$median_R0,
          h=input_list$stocks[[i]]$median_h,
          phi=om_stocks[[i]]$Phi.0,
          sigmaR=input_list$stocks[[i]]$logR_sd,
          mean2med=FALSE,
          model=input_list$stocks[[i]]$SRmodel)$R0BC

        input_list$stocks[[i]]$mean_h <- convertSRparms(
          R0=input_list$stocks[[i]]$median_R0,
          h=input_list$stocks[[i]]$median_h,
          phi=om_stocks[[i]]$Phi.0,
          sigmaR=input_list$stocks[[i]]$logR_sd,
          mean2med=FALSE,
          model=input_list$stocks[[i]]$SRmodel)$hBC
      }


      ## Input data list
      temp <- list(
        recruit_transportation=input_list$stocks[[i]]$recruit_transportation,
        movement_matrix=input_list$stocks[[i]]$movement_matrix,
        fleet_num=input_list$stocks[[i]]$fleet_num,
        survey_num=input_list$stocks[[i]]$survey_num,
        nyr=nyr,
        year=input_list$stocks[[i]]$year,
        ages=input_list$stocks[[i]]$ages,
        nages=nages,
        cv.L=input_list$stocks[[i]]$cv.L,
        cv.survey=input_list$stocks[[i]]$cv.survey,
        n.L=input_list$stocks[[i]]$n.L,
        n.survey=input_list$stocks[[i]]$n.survey,
        logR_sd=input_list$stocks[[i]]$logR_sd,
        logf_sd=input_list$stocks[[i]]$logf_sd,
        om_bias_cor=input_list$stocks[[i]]$om_bias_cor,
        bias_cor_method=input_list$stocks[[i]]$bias_cor_method,
        R0=ifelse((input_list$stocks[[i]]$om_bias_cor==TRUE & input_list$stocks[[i]]$bias_cor_method == "mean_unbiased"), input_list$stocks[[i]]$mean_R0, input_list$stocks[[i]]$median_R0),
        h=ifelse((input_list$stocks[[i]]$om_bias_cor==TRUE & input_list$stocks[[i]]$bias_cor_method == "mean_unbiased"), input_list$stocks[[i]]$mean_h, input_list$stocks[[i]]$median_h),
        median_R0=input_list$stocks[[i]]$median_R0,
        median_h=input_list$stocks[[i]]$median_h,
        mean_R0=input_list$stocks[[i]]$mean_R0,
        mean_h=input_list$stocks[[i]]$mean_h,
        SRmodel=input_list$stocks[[i]]$SRmodel,
        M=input_list$stocks[[i]]$M,
        Linf=input_list$stocks[[i]]$Linf,
        K=input_list$stocks[[i]]$K,
        a0=input_list$stocks[[i]]$a0,
        a.lw=input_list$stocks[[i]]$a.lw,
        b.lw=input_list$stocks[[i]]$b.lw,
        A50.mat=input_list$stocks[[i]]$A50.mat,
        slope.mat=input_list$stocks[[i]]$slope.mat,
        sel_fleet=input_list$stocks[[i]]$sel_fleet,
        sel_survey=input_list$stocks[[i]]$sel_survey,
        len=om_stocks[[i]]$len,
        W.kg=om_stocks[[i]]$W.kg,
        W.mt=om_stocks[[i]]$W.mt,
        M.age=om_stocks[[i]]$M.age,
        mat.age=om_stocks[[i]]$mat.age,
        proportion.female=om_stocks[[i]]$proportion.female,
        selex_fleet=om_stocks[[i]]$selex_fleet,
        selex_survey=om_stocks[[i]]$selex_survey,
        N.pr0=om_stocks[[i]]$N.pr0,
        Phi.0=om_stocks[[i]]$Phi.0,
        logR.resid=om_stocks[[i]]$logR.resid,
        logf.resid=om_stocks[[i]]$logf.resid,
        f=om_stocks[[i]]$f,
        initial_equilibrium_F=input_list$stocks[[i]]$initial_equilibrium_F,
        brp_f_vector = input_list$stocks[[i]]$brp_f_vector,
        brp_f_option = input_list$stocks[[i]]$brp_f_option)

      om_input[[i]] <- temp
    }


    om_output <- popsim(x=om_input)

    for(i in 1:length(input_list$stocks)){
      if(input_list$stock[[i]]$initial_equilibrium_F==FALSE){
        om_input[[i]]$year <<- om_input[[i]]$year[1]:(om_input[[i]]$year[length(om_input[[i]]$year)]-1)
        om_input[[i]]$nyr <<- length(om_input[[i]]$year)
        input_list[[i]]$year <- input_list[[i]]$year[1]:(input_list[[i]]$year[length(input_list[[i]]$year)]-1)
      }
    }

    ## Simulate the survey index
    survey_data <- vector(mode="list", length=length(input_list$stocks))
    for(i in 1:length(survey_data)){
      survey_data[[i]]$survey_age_comp <- vector(mode="list", length=input_list$stocks[[i]]$survey_num)
      names(survey_data[[i]]$survey_age_comp) <- paste("survey", 1:input_list$stocks[[i]]$survey_num, sep="")

      survey_data[[i]]$survey_index <- vector(mode="list", length=input_list$stocks[[i]]$survey_num)
      names(survey_data[[i]]$survey_index) <- paste("survey", 1:input_list$stocks[[i]]$survey_num, sep="")

      survey_data[[i]]$survey_q <- vector(mode="list", length=input_list$stocks[[i]]$survey_num)
      names(survey_data[[i]]$survey_q) <- paste("survey", 1:input_list$stocks[[i]]$survey_num, sep="")

      for (x in 1:length(survey_data[[i]]$survey_age_comp)) {
        survey_data[[i]]$survey_age_comp[[x]] <- om_output$stocks[[i]]$N.age%*%diag(om_stocks[[i]]$selex_survey[[x]])
        survey_annual_sum <- rowSums(survey_data[[i]]$survey_age_comp[[x]])
        survey_data[[i]]$survey_index[[x]] <- survey_annual_sum/mean(survey_annual_sum)
        survey_data[[i]]$survey_q[[x]] <- 1/mean(survey_annual_sum)
      }

      om_output$stocks[[i]]$survey_age_comp <- survey_data[[i]]$survey_age_comp
      om_output$stocks[[i]]$survey_index <- survey_data[[i]]$survey_index
      om_output$stocks[[i]]$survey_q <- survey_data[[i]]$survey_q

    }

    ## Generate observation data
    temp <- vector(mode="list", length=length(input_list$stocks))
    for (i in 1:length(temp)){
      temp[[i]] <- ObsModel(nyr=om_input[[i]]$nyr,
                            nages=om_input[[i]]$nages,
                            fleet_num=om_input[[i]]$fleet_num,
                            L=om_output$stocks[[i]]$L.mt,
                            survey_num=om_input[[i]]$survey_num,
                            survey=om_output$stocks[[i]]$survey_index,
                            L.age=om_output$stocks[[i]]$L.age,
                            survey.age=om_output$stocks[[i]]$survey_age_comp,
                            cv.L=om_input[[i]]$cv.L,
                            cv.survey=om_input[[i]]$cv.survey,
                            n.L=om_input[[i]]$n.L,
                            n.survey=om_input[[i]]$n.survey)

      temp[[i]]$cv.L <- input_list$stocks[[i]]$input.cv.L
      temp[[i]]$cv.survey <- input_list$stocks[[i]]$input.cv.survey
      temp[[i]]$survey_q=om_output$stocks[[i]]$survey_q
    }
    em_input <- vector(mode="list", length=length(input_list$stocks))
    names(em_input) <- paste("stock", 1:length(input_list$stocks), sep="")
    em_input <- temp

    save(om_input, om_output, em_input, file=file.path(casedir, "output", subdir, paste("OM", om_sim, ".RData", sep="")))

    if (show_iter_num==TRUE) print(paste("Iteration", om_sim, "finished"))


  }
}
