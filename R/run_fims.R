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

  if (!("FIMS" %in% installed.packages()[, "Package"])) stop("Please install FIMS!")
  unlink(list.files(file.path(casedir, "output", "FIMS"), full.names = TRUE), recursive = TRUE)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep = ""))))

  for (om_sim in 1:om_sim_num) {
    load(file=file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    libs_path <- system.file("libs", package = "FIMS")
    dll_name <- paste("FIMS", .Platform$dynlib.ext, sep = "")
    if (.Platform$OS.type == "windows") {
      dll_path <- file.path(libs_path, .Platform$r_arch, dll_name)
    } else {
      dll_path <- file.path(libs_path, dll_name)
    }

    data(package = "FIMS")

    ## Set-up Rcpp modules and fix parameters to "true"
    fims <- Rcpp::Module("fims", PACKAGE = "FIMS")

    # Recruitment
    recruitment <- new(fims$BevertonHoltRecruitment)
    recruitment$log_sigma_recruit$value <- log(om_input$logR_sd)
    recruitment$rzero$value <- om_input$R0
    recruitment$rzero$is_random_effect <- FALSE
    recruitment$rzero$estimated <- TRUE
    recruitment$steep$value <- om_input$h
    recruitment$steep$is_random_effect <- FALSE
    recruitment$steep$estimated <- TRUE
    recruitment$estimate_deviations <- TRUE
    recruitment$deviations <- exp(om_input$logR.resid)

    # Growth
    ewaa_growth <- new(fims$EWAAgrowth)
    age_frame <- FIMS::FIMSFrameAge(data_mile1)
    # ewaa_growth$ages <- FIMS::m_ages(age_frame)
    # ewaa_growth$weights <- FIMS::m_weightatage(age_frame)
    ewaa_growth$ages <- age_frame@ages
    ewaa_growth$weights <- dplyr::filter(
      .data = as.data.frame(age_frame@data),
      .data[["type"]] == "weight-at-age"
    ) %>%
      dplyr::group_by(.data[["age"]]) %>%
      dplyr::summarize(mean_value = mean(.data[["value"]])) %>%
      dplyr::pull(.data[["mean_value"]])

    # Maturity
    maturity <- new(fims$LogisticMaturity)
    maturity$median$value <- om_input$A50.mat
    maturity$median$is_random_effect <- FALSE
    maturity$median$estimated <- TRUE
    maturity$slope$value <- om_input$slope
    maturity$slope$is_random_effect <- FALSE
    maturity$slope$estimated <- TRUE

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
    fishing_fleet$log_q <- rep(log(1.0), om_input$nyr)
    fishing_fleet$estimate_q <- TRUE
    fishing_fleet$random_q <- FALSE
    fishing_fleet$SetAgeCompLikelihood(1)
    fishing_fleet$SetIndexLikelihood(1)
    fishing_fleet$SetObservedAgeCompData(1, as.matrix(c(t(em_input$L.age.obs$fleet1))))
    fishing_fleet$SetObservedIndexData(1, em_input$L.obs$fleet1)
    fishing_fleet$SetSelectivity(fishing_fleet_selectivity$get_id())

    # Create the survey fleet
    survey_fleet_selectivity <- new(fims$LogisticSelectivity)
    survey_fleet_selectivity$median$value <- om_input$sel_survey$survey1$A50.sel1
    survey_fleet_selectivity$median$is_random_effect <- FALSE
    survey_fleet_selectivity$median$estimated <- TRUE
    survey_fleet_selectivity$slope$value <- om_input$sel_survey$survey1$slope.sel1
    survey_fleet_selectivity$slope$is_random_effect <- FALSE
    survey_fleet_selectivity$slope$estimated <- TRUE

    survey_fleet <- new(fims$Fleet)
    survey_fleet$nages <- om_input$nages
    survey_fleet$nyears <- om_input$nyr
    survey_fleet$log_Fmort <- rep(log(0.0), om_input$nyr) #-Inf?
    survey_fleet$estimate_F <- TRUE
    survey_fleet$random_F <- FALSE
    survey_fleet$log_q <- rep(log(om_output$survey_q$survey1), om_input$nyr)
    survey_fleet$estimate_q <- TRUE
    survey_fleet$random_q <- FALSE
    survey_fleet$SetAgeCompLikelihood(1)
    survey_fleet$SetIndexLikelihood(1)
    survey_fleet$SetObservedAgeCompData(2, as.matrix(c(t(em_input$survey.age.obs$survey1))))
    survey_fleet$SetObservedIndexData(2, em_input$survey.obs$survey1)
    survey_fleet$SetSelectivity(survey_fleet_selectivity$get_id())

    # Population
    population <- new(fims$Population)
    # is it a problem these are not Parameters in the Population interface?
    # the Parameter class (from rcpp/rcpp_objects/rcpp_interface_base) cannot handle vectors, do we need a ParameterVector class?
    population$log_M <- rep(log(om_input$M.age), om_input$nyr*om_input$nages)
    population$log_init_naa <- log(om_output$N.age[1, ])
    population$nages <- om_input$nages
    population$ages <- om_input$ages * 1.0
    population$nfleets <- sum(om_input$fleet_num, om_input$survey_num)
    population$nseasons <- 1
    population$nyears <- om_input$nyr
    population$prop_female <- om_input$proportion.female[1]
    population$SetMaturity(maturity$get_id())
    population$SetGrowth(ewaa_growth$get_id())
    population$SetRecruitment(recruitment$get_id())

    ## Set-up TMB
    fims$CreateTMBModel()
    # Create parameter list from Rcpp modules
    parameters <- list(p = fims$get_fixed())
    obj <- TMB::MakeADFun(data=list(), parameters, DLL="FIMS")

    output_file <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), paste("s", om_sim, ".RData", sep = ""))
    save(obj, file=output_file)
    fims$clear()

    dyn.unload(dll_path)
    dyn.load(dll_path)
    rm(obj)
  }
}
