run_om <- function(maindir=NULL,
                   subdir="OM",...){

  ## Set up required working folders (output and figure)
  if (is.null(maindir)) stop ("Missing main working directory!")

  invisible(sapply(c("output", "figure"), function(x) {
    if (!file.exists(file.path(maindir, x))) dir.create(file.path(maindir, x))
  }))

  invisible(sapply(c("OM"), function(x) {
    if (!file.exists(file.path(maindir, "output", x))) dir.create(file.path(maindir, "output", x))
  }))

  invisible(do.call(file.remove, list(list.files(file.path(maindir, "output", "OM"), full.names = TRUE))))

  if (is.null(seed_num)) {
    set.seed(9924)
  } else {
    set.seed(seed_num)
  }

  nyr <<- length(year)
  nages <<- length(ages)

  ## Set up F deviations-at-age per iteration
  f_dev_matrix <- f_dev_case(f_dev_change,
                             nyr,
                             logf_sd,
                             om_sim_num)

  f_matrix <- f_case(f_pattern,
                     f_min,
                     f_max,
                     f_end,
                     f_mean,
                     start_val,
                     start_year,
                     nyr,
                     om_sim_num,
                     f_dev_matrix)

  ## Set up R deviations-at-age per iteration
  r_dev_matrix <- r_dev_case(r_dev_change, nyr, logR_sd, om_sim_num)

  ## Create and fill vectors to be used in the population model
  len=Linf*(1-exp(-K*(ages-a0))) #von Bertalanffy growth
  W.kg=a.lw*len^b.lw  #Weight-length relationship, assumed to be in kg
  W.mt=W.kg/1000 #Weight in mt
  M.age=rep(M,nages) #natural mortality at age
  mat.age=logistic(pattern=pattern.mat, ages, slope.mat, A50.mat) #maturity at age
  proportion.female=rep(female.proportion, nages) #proportion female at age

  setwd(paste(maindir))

  for(om_sim in 1:om_sim_num){
    logR.resid <- r_dev_matrix[om_sim,]
    logf.resid <- f_dev_matrix[om_sim,]
    f <- f_matrix[om_sim,]

    ## Selectivity for fleets
    selex_fleet <- vector(mode="list", length=fleet_num)
    names(selex_fleet) <- paste("fleet", 1:fleet_num, sep="")
    invisible(sapply(1:length(selex_fleet), function(x)
      selex_fleet[[x]] <<- logistic(sel_fleet[[x]]$pattern,
                                    ages,
                                    sel_fleet[[x]]$slope.sel,
                                    sel_fleet[[x]]$A50.sel)))

    ## Selectivity for surveys
    selex_survey <- vector(mode="list", length=survey_num)
    names(selex_survey) <- paste("survey", 1:survey_num, sep="")
    invisible(sapply(1:length(selex_survey), function(x)
      selex_survey[[x]] <<- logistic(sel_survey[[x]]$pattern,
                                     ages,
                                     sel_survey[[x]]$slope.sel,
                                     sel_survey[[x]]$A50.sel)))

    ## Compute the number of spawners per recruit of an unfished population (Phi.0)
    N.pr0=rep(1,nages) #Number of spawners per recruit at age
    for (a in 1:(nages-1)) {
      N.pr0[a+1]=N.pr0[a]*exp(-M.age[a])
    }
    N.pr0[nages]=N.pr0[nages]/(1-exp(-M.age[nages]))  #Plus group
    Phi.0=sum(N.pr0*proportion.female*mat.age*W.mt)     #Spawners per recruit based on mature female biomass

    ## Input data list
    om_input <<- list(fleet_num=fleet_num,
                     survey_num=survey_num,
                     nyr=nyr,
                     year=year,
                     ages=ages,
                     nages=nages,
                     cv.L=cv.L,
                     cv.survey=cv.survey,
                     n.L=n.L,
                     n.survey=n.survey,
                     logR_sd=logR_sd,
                     logf_sd=logf_sd,
                     R0=R0,
                     h=h,
                     M=M,
                     Linf=Linf,
                     K=K,
                     a0=a0,
                     a.lw=a.lw,
                     b.lw=b.lw,
                     A50.mat=A50.mat,
                     slope.mat=slope.mat,
                     sel_fleet=sel_fleet,
                     sel_survey=sel_survey,
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

    ## OM input data for simulate stock dynamics
    input<-list(nyr=nyr,
                f=f,
                ages=ages,
                nages=nages,
                R0=R0,
                h=h,
                Phi.0=Phi.0,
                M.age=M.age,
                W.mt=W.mt,
                mat.age=mat.age,
                prop.f=proportion.female,
                selex_fleet=selex_fleet,
                logR.resid=logR.resid)

    om_output <<- popsim(x=input)

    ## Simulate the survey index
    survey_age_comp <- vector(mode="list", length=survey_num)
    names(survey_age_comp) <- paste("survey", 1:survey_num, sep="")
    survey_index <- vector(mode="list", length=survey_num)
    names(survey_index) <- paste("survey", 1:survey_num, sep="")
    survey_q <- vector(mode="list", length=survey_num)
    names(survey_q) <- paste("survey", 1:survey_num, sep="")

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
    em_input <<- ObsModel(L=om_output$L.mt,
                      survey=om_output$survey_index,
                      L.age=om_output$L.age,
                      survey.age=om_output$survey_age_comp,
                      cv.L=cv.L,
                      cv.survey=cv.survey,
                      n.L=n.L,
                      n.survey=n.survey)

    em_input$cv.L <<- input.cv.L
    em_input$cv.survey <<- input.cv.survey

    save(om_input, om_output, em_input, file=file.path(maindir, "output", subdir, paste("OM", om_sim, ".RData", sep="")))
  }
}






