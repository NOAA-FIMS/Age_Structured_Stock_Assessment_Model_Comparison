#' Function to run Fisheries Integrated Modeling System (https://github.com/NOAA-FIMS/FIMS)
#' @name run_fims
#' @description Function to read simulated true values from the operating model and to run FIMS
#' @param maindir Main working directory
#' @param subdir Estimation model working directory
#' @param om_sim_num Number of iterations from the operating model
#' @param casedir Case working directory
#' @param em_bias_cor Use bias correction in the estimation model or not?
#' @export
run_fims <- function(
  maindir = maindir,
  subdir = "FIMS",
  om_sim_num = NULL,
  casedir = casedir,
  em_bias_cor = em_bias_cor) {

  # if (!("FIMS" %in% installed.packages()[, "Package"])) stop("Please install FIMS!")
  unlink(list.files(file.path(casedir, "output", "FIMS"), full.names = TRUE), recursive = TRUE)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep = ""))))

  for (om_sim in 1:om_sim_num) {
    print(om_sim)
    devtools::load_all("C:\\Users\\bai.li\\Documents\\FIMS")
    library(FIMS)

    # dll_path <- "C:\\Users\\bai.li\\Documents\\FIMS\\src\\FIMS.dll"
    # dyn.load(dll_path)

    load(file=file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    # libs_path <- system.file("libs", package = "FIMS")
    # dll_name <- paste("FIMS", .Platform$dynlib.ext, sep = "")
    # if (.Platform$OS.type == "windows") {
    #   dll_path <- file.path(libs_path, .Platform$r_arch, dll_name)
    # } else {
    #   dll_path <- file.path(libs_path, dll_name)
    # }

    data(package = "FIMS")

    ## Set-up Rcpp modules and fix parameters to "true"
    fims <- Rcpp::Module("fims", PACKAGE = "FIMS")

    # Recruitment
    recruitment <- new(fims$BevertonHoltRecruitment)
    recruitment$log_sigma_recruit$value <- log(om_input$logR_sd)
    recruitment$log_rzero$value <- 12.0#log(om_input$R0)
    recruitment$log_rzero$is_random_effect <- FALSE
    recruitment$log_rzero$estimated <- TRUE
    recruitment$logit_steep$value <- -log(1.0 - om_input$h) + log(om_input$h - 0.2)
    recruitment$logit_steep$is_random_effect <- FALSE
    recruitment$logit_steep$estimated <- FALSE
    # recruitment$logit_steep$value<-0.75
    recruitment$logit_steep$min <- 0.2
    recruitment$logit_steep$max <- 1.0
    recruitment$estimate_deviations <- TRUE
    recruitment$deviations <- rep(1, length(om_input$logR.resid))

    #Data
    catch <- em_input$L.obs$fleet1
    fishing_fleet_index <- new(fims$Index, length(catch))
    fishing_fleet_index$index_data <- catch
    fishing_fleet_age_comp <- new(fims$AgeComp, length(catch), om_input$nages)
    fishing_fleet_age_comp$age_comp_data <- c(t(em_input$L.age.obs$fleet1)) * 200


    # survey_index <-em_input$survey.obs$survey1
    survey_index <-em_input$surveyB.obs$survey1
    survey_fleet_index <- new(fims$Index, length(survey_index))
    survey_fleet_index$index_data <- survey_index
    survey_fleet_age_comp <- new(fims$AgeComp, length(survey_index), om_input$nages)
    survey_fleet_age_comp$age_comp_data <-c(t(em_input$survey.age.obs$survey1)) *200


    # Growth
    ewaa_growth <- new(fims$EWAAgrowth)
    ewaa_growth$ages <- om_input$ages
    ewaa_growth$weights <- om_input$W.mt

    # Maturity
    maturity <- new(fims$LogisticMaturity)
    maturity$median$value <- om_input$A50.mat
    maturity$median$is_random_effect <- FALSE
    maturity$median$estimated <- FALSE
    maturity$slope$value <- om_input$slope
    maturity$slope$is_random_effect <- FALSE
    maturity$slope$estimated <- FALSE

    # Fleet
    # Create the fishing fleet
    fishing_fleet_selectivity <- new(fims$LogisticSelectivity)
    fishing_fleet_selectivity$median$value <- om_input$sel_fleet$fleet1$A50.sel1
    fishing_fleet_selectivity$median$is_random_effect <- FALSE
    fishing_fleet_selectivity$median$estimated <- TRUE
    fishing_fleet_selectivity$slope$value <- om_input$sel_fleet$fleet1$slope.sel1
    fishing_fleet_selectivity$slope$is_random_effect <- FALSE
    fishing_fleet_selectivity$slope$estimated <- TRUE

    fishing_fleet <- new(fims$Fleet)
    # Need get_id() for setting up observed agecomp and index data?
    fishing_fleet$nages <- om_input$nages
    fishing_fleet$nyears <- om_input$nyr
    fishing_fleet$log_Fmort <- log(om_output$f)
    fishing_fleet$estimate_F <- TRUE
    fishing_fleet$random_F <- FALSE
    fishing_fleet$log_q <- log(1.0)
    fishing_fleet$estimate_q <- FALSE
    fishing_fleet$random_q <- FALSE
    fishing_fleet$log_obs_error$value <- log(sqrt(log(em_input$cv.L$fleet1^2+1)))
    fishing_fleet$log_obs_error$estimated <- FALSE
    fishing_fleet$SetAgeCompLikelihood(1)
    fishing_fleet$SetIndexLikelihood(1)
    fishing_fleet$SetSelectivity(fishing_fleet_selectivity$get_id())
    fishing_fleet$SetObservedIndexData(fishing_fleet_index$get_id())
    fishing_fleet$SetObservedAgeCompData(fishing_fleet_age_comp$get_id())

    # Create the survey fleet
    survey_fleet_selectivity <- new(fims$LogisticSelectivity)
    survey_fleet_selectivity$median$value <- om_input$sel_survey$survey1$A50.sel1
    survey_fleet_selectivity$median$is_random_effect <- FALSE
    survey_fleet_selectivity$median$estimated <- TRUE
    survey_fleet_selectivity$slope$value <- om_input$sel_survey$survey1$slope.sel1
    survey_fleet_selectivity$slope$is_random_effect <- FALSE
    survey_fleet_selectivity$slope$estimated <- TRUE

    survey_fleet <- new(fims$Fleet)
    survey_fleet$is_survey<-TRUE
    survey_fleet$nages <- om_input$nages
    survey_fleet$nyears <- om_input$nyr
    #survey_fleet$log_Fmort <- rep(log(0.0000000000000000000000000001), om_input$nyr) #-Inf?
    survey_fleet$estimate_F <- FALSE
    survey_fleet$random_F <- FALSE
    survey_fleet$log_q <- log(om_output$survey_q$survey1)
    survey_fleet$estimate_q <- TRUE
    survey_fleet$random_q <- FALSE
    survey_fleet$log_obs_error$value <- log(sqrt(log(em_input$cv.survey$survey1^2+1)))
    survey_fleet$log_obs_error$estimated <- FALSE
    survey_fleet$SetAgeCompLikelihood(1)
    survey_fleet$SetIndexLikelihood(1)
    survey_fleet$SetSelectivity(survey_fleet_selectivity$get_id())
    survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
    survey_fleet$SetObservedAgeCompData(survey_fleet_age_comp$get_id())

    # Population
    population <- new(fims$Population)
    # is it a problem these are not Parameters in the Population interface?
    # the Parameter class (from rcpp/rcpp_objects/rcpp_interface_base) cannot handle vectors, do we need a ParameterVector class?
    population$log_M <- rep(log(om_input$M.age[1]), om_input$nyr*om_input$nages)
    population$estimate_M <- FALSE
    population$log_init_naa <- log(om_output$N.age[1, ])
    population$estimate_init_naa <- TRUE
    population$nages <- om_input$nages
    population$ages <- om_input$ages * 1.0
    population$nfleets <- sum(om_input$fleet_num, om_input$survey_num)
    population$nseasons <- 1
    population$nyears <- om_input$nyr
    population$prop_female <- om_input$proportion.female[1]
    population$SetMaturity(maturity$get_id())
    population$SetGrowth(ewaa_growth$get_id())
    population$SetRecruitment(recruitment$get_id())

    # # Recruitment
    # recruitment <- new(fims$BevertonHoltRecruitment)
    # # log_sigma_recruit is NOT logged. It needs to enter the model logged b/c the exp() is taken before the likelihood calculation
    # # recruitment$log_sigma_recruit$value <- log(om_input$logR_sd)
    # recruitment$log_sigma_recruit$value <- om_input$logR_sd
    # recruitment$log_rzero$value <- 12.0#log(om_input$R0)
    # recruitment$log_rzero$is_random_effect <- FALSE
    # recruitment$log_rzero$estimated <- TRUE
    # recruitment$logit_steep$value <- -log(1.0 - om_input$h) + log(om_input$h - 0.2)
    # recruitment$logit_steep$is_random_effect <- FALSE
    # recruitment$logit_steep$estimated <- FALSE
    # # recruitment$logit_steep$value<-0.75
    # recruitment$logit_steep$min <- 0.2
    # recruitment$logit_steep$max <- 1.0
    # recruitment$estimate_deviations <- TRUE
    # #recruit deviations should enter the model in normal space. The log is taken in the likelihood calculations
    # recruitment$deviations <- rep(1, length(om_input$logR.resid))
    #
    # #Data
    # catch <- em_input$surveyB.obs$survey1
    # fishing_fleet_index <- new(fims$Index, length(catch))
    # fishing_fleet_index$index_data <- catch
    # fishing_fleet_age_comp <- new(fims$AgeComp, length(catch), om_input$nages)
    # fishing_fleet_age_comp$age_comp_data <- c(t(em_input$L.age.obs$fleet1)) * 200
    #
    #
    # survey_index <-em_input$survey.obs$survey1
    # survey_fleet_index <- new(fims$Index, length(survey_index))
    # survey_fleet_index$index_data <- survey_index
    # survey_fleet_age_comp <- new(fims$AgeComp, length(survey_index), om_input$nages)
    # survey_fleet_age_comp$age_comp_data <-c(t(em_input$survey.age.obs$survey1)) *200
    #
    #
    # # Growth
    # ewaa_growth <- new(fims$EWAAgrowth)
    # ewaa_growth$ages <- om_input$ages
    # ewaa_growth$weights <- om_input$W.mt
    #
    # # Maturity
    # maturity <- new(fims$LogisticMaturity)
    # maturity$median$value <- om_input$A50.mat
    # maturity$median$is_random_effect <- FALSE
    # maturity$median$estimated <- FALSE
    # maturity$slope$value <- om_input$slope
    # maturity$slope$is_random_effect <- FALSE
    # maturity$slope$estimated <- FALSE
    #
    # # Fleet
    # # Create the fishing fleet
    # fishing_fleet_selectivity <- new(fims$LogisticSelectivity)
    # fishing_fleet_selectivity$median$value <- om_input$sel_fleet$fleet1$A50.sel1
    # fishing_fleet_selectivity$median$is_random_effect <- FALSE
    # fishing_fleet_selectivity$median$estimated <- TRUE
    # fishing_fleet_selectivity$slope$value <- om_input$sel_fleet$fleet1$slope.sel1
    # fishing_fleet_selectivity$slope$is_random_effect <- FALSE
    # fishing_fleet_selectivity$slope$estimated <- TRUE
    #
    # fishing_fleet <- new(fims$Fleet)
    # # Need get_id() for setting up observed agecomp and index data?
    # fishing_fleet$nages <- om_input$nages
    # fishing_fleet$nyears <- om_input$nyr
    # fishing_fleet$log_Fmort <- log(om_output$f)
    # fishing_fleet$estimate_F <- TRUE
    # fishing_fleet$random_F <- FALSE
    # fishing_fleet$log_q <- log(1.0)
    # fishing_fleet$estimate_q <- FALSE
    # fishing_fleet$random_q <- FALSE
    # fishing_fleet$log_obs_error$value <- log(sqrt(log(em_input$cv.L$fleet1^2+1)))
    # fishing_fleet$log_obs_error$estimated <- FALSE
    # fishing_fleet$SetAgeCompLikelihood(1)
    # fishing_fleet$SetIndexLikelihood(1)
    # fishing_fleet$SetSelectivity(fishing_fleet_selectivity$get_id())
    # fishing_fleet$SetObservedIndexData(fishing_fleet_index$get_id())
    # fishing_fleet$SetObservedAgeCompData(fishing_fleet_age_comp$get_id())
    #
    # # Create the survey fleet
    # survey_fleet_selectivity <- new(fims$LogisticSelectivity)
    # survey_fleet_selectivity$median$value <- om_input$sel_survey$survey1$A50.sel1
    # survey_fleet_selectivity$median$is_random_effect <- FALSE
    # survey_fleet_selectivity$median$estimated <- TRUE
    # survey_fleet_selectivity$slope$value <- om_input$sel_survey$survey1$slope.sel1
    # survey_fleet_selectivity$slope$is_random_effect <- FALSE
    # survey_fleet_selectivity$slope$estimated <- TRUE
    #
    # survey_fleet <- new(fims$Fleet)
    # survey_fleet$is_survey<-TRUE
    # survey_fleet$nages <- om_input$nages
    # survey_fleet$nyears <- om_input$nyr
    # #survey_fleet$log_Fmort <- rep(log(0.0000000000000000000000000001), om_input$nyr) #-Inf?
    # survey_fleet$estimate_F <- FALSE
    # survey_fleet$random_F <- FALSE
    # survey_fleet$log_q <- log(om_output$survey_q$survey1)
    # survey_fleet$estimate_q <- TRUE
    # survey_fleet$random_q <- FALSE
    # # survey_fleet$log_obs_error$value <- log(em_input$cv.survey$survey1)
    # survey_fleet$log_obs_error$value <- log(sqrt(log(em_input$cv.survey$survey1^2+1)))
    # survey_fleet$log_obs_error$estimated <- FALSE
    # survey_fleet$SetAgeCompLikelihood(1)
    # survey_fleet$SetIndexLikelihood(1)
    # survey_fleet$SetSelectivity(survey_fleet_selectivity$get_id())
    # survey_fleet$SetObservedIndexData(survey_fleet_index$get_id())
    # survey_fleet$SetObservedAgeCompData(survey_fleet_age_comp$get_id())
    #
    # # Population
    # population <- new(fims$Population)
    # # is it a problem these are not Parameters in the Population interface?
    # # the Parameter class (from rcpp/rcpp_objects/rcpp_interface_base) cannot handle vectors, do we need a ParameterVector class?
    # population$log_M <- rep(log(om_input$M.age[1]), om_input$nyr*om_input$nages)
    # population$estimate_M <- FALSE
    # population$log_init_naa <- log(om_output$N.age[1, ])
    # population$estimate_init_naa <- TRUE
    # population$nages <- om_input$nages
    # population$ages <- om_input$ages * 1.0
    # population$nfleets <- sum(om_input$fleet_num, om_input$survey_num)
    # population$nseasons <- 1
    # population$nyears <- om_input$nyr
    # population$prop_female <- om_input$proportion.female[1]
    # population$SetMaturity(maturity$get_id())
    # population$SetGrowth(ewaa_growth$get_id())
    # population$SetRecruitment(recruitment$get_id())

    ## Set-up TMB
    fims$CreateTMBModel()
    # # Create parameter list from Rcpp modules
    parameters <- list(p = fims$get_fixed())
    par_list <- 1:65
    par_list[c(32:65)] <- NA
    map <- list(p=factor(par_list))
    obj <- MakeADFun(data=list(), parameters, DLL="FIMS")#, map = map)
    obj$gr(obj$par)
    p = fims$get_fixed()

    try(opt<- with(obj,optim(par, fn, gr, method = "BFGS", control = list(maxit=1000000, reltol = 1e-15))), silent = TRUE)

    opt$par
    obj$gr(opt$par)
    sdr <- TMB::sdreport(obj)
    summary(sdr, "fixed")
    summary(sdr, "report")
    message("success!")
    report <- obj$report()
    output_file <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), paste("s", om_sim, ".RData", sep = ""))
    save(report, file=output_file)
    max_gradient <- max(abs(obj$gr(opt$par)))
    fims_gradient <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), paste("s", om_sim, "_gradient.RData", sep = ""))
    save(max_gradient, file=fims_gradient)
    fims$clear()

    # dll_path <- "C:\\Users\\bai.li\\Documents\\FIMS\\src\\FIMS.dll"
    # try(dll_path <- "C:\\Users\\bai.li\\Desktop\\FIMS\\src\\FIMS.dll")
    # dyn.unload(dll_path)
    # dyn.load(dll_path)
  }
}
