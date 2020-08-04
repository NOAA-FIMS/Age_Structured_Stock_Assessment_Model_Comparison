run_om <- function(input_list=NULL,
                   show_iter_num=FALSE){
  maindir <- input_list$maindir
  subdir <- "OM"
  case_name <- input_list$case_name
  ## Error checks for missing files
  if (is.null(maindir)) stop ("Missing main working directory!")
  if (!file.exists(file.path(maindir, "em_input"))) stop ("Missing estimation model input file!")

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

  nyr <- length(input_list$year)
  nages <- length(input_list$ages)

  ## Set up F deviations-at-age per iteration
  f_dev_matrix <- f_dev_case(f_dev_change=input_list$f_dev_change,
                             nyr=nyr,
                             logf_sd=input_list$logf_sd,
                             om_sim_num=input_list$om_sim_num)

  f_matrix <- f_case(f_pattern=input_list$f_pattern,
                     start_val=input_list$start_val,
                     middle_val=input_list$middle_val,
                     end_val=input_list$end_val,
                     f_val=input_list$f_val,
                     start_year=input_list$start_year,
                     middle_year=input_list$middle_year,
                     nyr=nyr,
                     om_sim_num=input_list$om_sim_num,
                     f_dev_matrix=f_dev_matrix)

  ## Set up R deviations-at-age per iteration
  r_dev_matrix <- r_dev_case(r_dev_change=input_list$r_dev_change,
                             nyr=nyr,
                             logR_sd=input_list$logR_sd,
                             om_sim_num=input_list$om_sim_num)

  ## Create and fill vectors to be used in the population model
  len=input_list$Linf*(1-exp(-input_list$K*(input_list$ages-input_list$a0))) #von Bertalanffy growth
  W.kg=input_list$a.lw*len^input_list$b.lw  #Weight-length relationship, assumed to be in kg
  W.mt=W.kg/1000 #Weight in mt
  M.age=rep(input_list$M,nages) #natural mortality at age
  mat.age=logistic(pattern=input_list$pattern.mat,
                   x=input_list$ages,
                   a1=input_list$slope.mat,
                   b1=input_list$A50.mat) #maturity at age
  proportion.female=rep(input_list$female.proportion, nages) #proportion female at age

  setwd(paste(maindir))

  for(om_sim in 1:om_sim_num){
    logR.resid <- r_dev_matrix[om_sim,]
    logf.resid <- f_dev_matrix[om_sim,]
    f <- f_matrix[om_sim,]

    ## Selectivity for fleets
    selex_fleet <- list(as.vector(sapply(1:length(input_list$fleet_num), function(x)
                               logistic(pattern=input_list$sel_fleet[[x]]$pattern,
                               x=input_list$ages,
                               a1=input_list$sel_fleet[[x]]$slope.sel1,
                               b1=input_list$sel_fleet[[x]]$A50.sel1,
                               a2=input_list$sel_fleet[[x]]$slope.sel2,
                               b2=input_list$sel_fleet[[x]]$A50.sel2))))
    names(selex_fleet) <- paste("fleet", 1:input_list$fleet_num, sep="")

    ## Selectivity for surveys
    selex_survey <- list(as.vector(sapply(1:length(input_list$survey_num), function(x)
                                logistic(input_list$sel_survey[[x]]$pattern,
                                input_list$ages,
                                a1=input_list$sel_survey[[x]]$slope.sel1,
                                b1=input_list$sel_survey[[x]]$A50.sel1,
                                a2=input_list$sel_survey[[x]]$slope.sel2,
                                b2=input_list$sel_survey[[x]]$A50.sel2))))
    names(selex_survey) <- paste("survey", 1:input_list$survey_num, sep="")

    ## Compute the number of spawners per recruit of an unfished population (Phi.0)
    N.pr0=rep(1,nages) #Number of spawners per recruit at age
    for (a in 1:(nages-1)) {
      N.pr0[a+1]=N.pr0[a]*exp(-M.age[a])
    }
    N.pr0[nages]=N.pr0[nages]/(1-exp(-M.age[nages]))  #Plus group
    Phi.0=sum(N.pr0*proportion.female*mat.age*W.mt)     #Spawners per recruit based on mature female biomass

    ## Convert mean R0 and mean h to median R0 and median h
    if(is.null(input_list$median_R0) | is.null(input_list$median_h)) {
     input_list$median_R0 <- convertSRparms(R0=input_list$mean_R0,
                                 h=input_list$mean_h,
                                 phi=Phi.0,
                                 sigmaR=input_list$logR_sd,
                                 mean2med=FALSE)$R0BC
     input_list$median_h <- convertSRparms(R0=input_list$mean_R0,
                                 h=input_list$mean_h,
                                 phi=Phi.0,
                                 sigmaR=input_list$logR_sd,
                                 mean2med=FALSE)$hBC
    } else {
     input_list$mean_R0 <- convertSRparms(R0=input_list$median_R0,
                                          h=input_list$median_h,
                                          phi=Phi.0,
                                          sigmaR=input_list$logR_sd,
                                          mean2med=TRUE)$R0BC
     input_list$mean_h <- convertSRparms(R0=input_list$median_R0,
                                          h=input_list$median_h,
                                          phi=Phi.0,
                                          sigmaR=input_list$logR_sd,
                                          mean2med=TRUE)$hBC
    }

    ## Input data list
    om_input <<- list(fleet_num=input_list$fleet_num,
                     survey_num=input_list$survey_num,
                     nyr=nyr,
                     year=input_list$year,
                     ages=input_list$ages,
                     nages=nages,
                     cv.L=input_list$cv.L,
                     cv.survey=input_list$cv.survey,
                     n.L=input_list$n.L,
                     n.survey=input_list$n.survey,
                     logR_sd=input_list$logR_sd,
                     logf_sd=input_list$logf_sd,
                     R0=input_list$median_R0,
                     h=input_list$median_h,
                     median_R0=input_list$median_R0,
                     median_h=input_list$median_h,
                     mean_R0=input_list$mean_R0,
                     mean_h=input_list$mean_h,
                     M=input_list$M,
                     Linf=input_list$Linf,
                     K=input_list$K,
                     a0=input_list$a0,
                     a.lw=input_list$a.lw,
                     b.lw=input_list$b.lw,
                     A50.mat=input_list$A50.mat,
                     slope.mat=input_list$slope.mat,
                     sel_fleet=input_list$sel_fleet,
                     sel_survey=input_list$sel_survey,
                     len=len,
                     W.kg=W.kg,
                     W.mt=W.mt,
                     M.age=M.age,
                     mat.age=mat.age,
                     proportion.female=proportion.female,
                     selex_fleet=selex_fleet,
                     selex_survey=selex_survey,
                     N.pr0=N.pr0,
                     Phi.0=Phi.0,
                     logR.resid=logR.resid,
                     logf.resid=logf.resid,
                     f=f)

    om_output <<- popsim(x=om_input)

    ## Simulate the survey index
    survey_age_comp <- vector(mode="list", length=input_list$survey_num)
    names(survey_age_comp) <- paste("survey", 1:input_list$survey_num, sep="")
    survey_index <- vector(mode="list", length=input_list$survey_num)
    names(survey_index) <- paste("survey", 1:input_list$survey_num, sep="")
    survey_q <- vector(mode="list", length=input_list$survey_num)
    names(survey_q) <- paste("survey", 1:input_list$survey_num, sep="")

    invisible(sapply(1:length(survey_age_comp), function(x) {
      survey_age_comp[[x]] <<- om_output$N.age%*%diag(selex_survey[[x]])
      survey_annual_sum <- rowSums(survey_age_comp[[x]])
      survey_index[[x]] <<- survey_annual_sum/mean(survey_annual_sum)
      survey_q[[x]] <<- 1/mean(survey_annual_sum)
    }))


    om_output$survey_age_comp <<- survey_age_comp
    om_output$survey_index <<- survey_index
    om_output$survey_q <<- survey_q

    ## Generate observation data
    em_input <<- ObsModel(nyr=om_input$nyr,
                         nages=om_input$nages,
                         fleet_num=om_input$fleet_num,
                         L=om_output$L.mt,
                         survey_num=om_input$survey_num,
                         survey=om_output$survey_index,
                         L.age=om_output$L.age,
                         survey.age=om_output$survey_age_comp,
                         cv.L=om_input$cv.L,
                         cv.survey=om_input$cv.survey,
                         n.L=om_input$n.L,
                         n.survey=om_input$n.survey)

    em_input$cv.L <<- input_list$input.cv.L
    em_input$cv.survey <<- input_list$input.cv.survey

    save(om_input, om_output, em_input, file=file.path(casedir, "output", subdir, paste("OM", om_sim, ".RData", sep="")))

    if (show_iter_num==TRUE) print(paste("Iteration", om_sim, "finished"))
  }
}
