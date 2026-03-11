#' Run the FIMS Estimation Model (EM)
#'
#' @description
#' This function iterates through a specified number of Operating Model (OM)
#' simulations, prepares the data, fits the FIMS model using TMB, and saves
#' the outputs (estimates and max gradient) to a simulation-specific subdirectory.
#'
#' @details
#' The core of this function is a `dplyr` pipeline that programmatically
#' updates the default FIMS parameters with the "true" values from the OM.
#'
#' @param maindir Path to the main working directory.
#' @param subdir The subdirectory within `casedir/output` where FIMS outputs
#'   will be saved. Defaults to `"FIMS"`.
#' @param om_sim_num The total number of OM simulations to process.
#' @param casedir The path to the specific case directory. This directory
#'   should contain an `output/OM` folder with OM simulation files (e.g.,
#'   `OM1.RData`, `OM2.RData`, ...).
#' @param em_bias_cor Bias correction factor for the EM.
#'
#' @return This function does not return an R object. It saves results to
#'   disk as side effects:
#'   \itemize{
#'     \item `s[sim_num].RData`: Contains the `output_fims` object.
#'     \item `s[sim_num]_gradient.RData`: Contains the `max_gradient_fims` object.
#'   }
#' @export
run_fims <- function(
  maindir = maindir,
  subdir = "FIMS",
  om_sim_num = NULL,
  casedir = casedir,
  em_bias_cor = em_bias_cor
) {
  # Check if FIMS is installed
  if (!("FIMS" %in% installed.packages()[, "Package"])) stop("Please install FIMS!")

  # Clean up previous FIMS outputs from the target directory to prevent conflicts
  unlink(list.files(file.path(casedir, "output", "FIMS"), full.names = TRUE), recursive = TRUE)
  # Create output directories for each simulation (e.g., .../FIMS/s1, .../FIMS/s2)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep = ""))))

  for (om_sim in 1:om_sim_num) {
    print(om_sim)
    # Load the specific OM simulation data (contains om_input, om_output, em_input)
    load(file = file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep = "")))

    # Convert OM/EM data into the required FIMSFrame format
    data_fims <- prepare_data_fims(
      om_input,
      om_output,
      em_input
    ) |>
      FIMS::FIMSFrame()

    # Configure parameters
    parameters <- data_fims |>
      # Initialize default FIMS configurations
      FIMS::create_default_configurations() |>
      # Create the default parameter list based on the data and configurations
      FIMS::create_default_parameters(data = data_fims) |>
      # Unnest the 'data' column to get a flat tibble of all parameters
      tidyr::unnest(col = data) |>
      # Update log_Fmort input values for Fleet1
      dplyr::rows_update(
        tibble::tibble(
          fleet_name = "fleet1",
          label = "log_Fmort",
          time = om_input[["year"]],
          value = log(om_output[["f"]])
        ),
        by = c("fleet_name", "label", "time")
      ) |>
      # Update selectivity parameters and log_q for survey1
      dplyr::rows_update(
        tibble::tibble(
          fleet_name = "survey1",
          label = c("inflection_point", "slope", "log_q"),
          value = c(
            om_input[["sel_survey"]][["survey1"]][["A50.sel1"]],
            om_input[["sel_survey"]][["survey1"]][["slope.sel1"]],
            # log(0.5)
            log(om_output[["survey_q"]][["survey1"]])
          )
        ),
        by = c("fleet_name", "label")
      ) |>
      # Update log_devs in the Recruitment module (time steps 2–30)
      dplyr::rows_update(
        tibble::tibble(
          label = "log_devs",
          time = om_input[["year"]][-1],
          value = om_input[["logR.resid"]][-1],
          estimation_type = "fixed_effects"
        ),
        by = c("label", "time")
      ) |>
      # Update log_sd for log_devs in the Recruitment module
      dplyr::rows_update(
        tibble::tibble(
          module_name = "Recruitment",
          label = "log_sd",
          value = om_input[["logR_sd"]]
        ),
        by = c("module_name", "label")
      ) |>
      # Update inflection point and slope parameters in the Maturity module
      dplyr::rows_update(
        tibble::tibble(
          module_name = "Maturity",
          label = c("inflection_point", "slope"),
          value = c(
            om_input[["A50.mat"]],
            om_input[["slope.mat"]]
          )
        ),
        by = c("module_name", "label")
      ) |>
      # Update log_init_naa values in the Population module
      dplyr::rows_update(
        tibble::tibble(
          label = "log_init_naa",
          age = om_input[["ages"]],
          value = log(om_output[["N.age"]][1, ])
        ),
        by = c("label", "age")
      ) |>
      # Update log_sd values for landings
      dplyr::rows_update(
        tibble::tibble(
          fleet_name = "fleet1",
          label = "log_sd",
          time = om_input[["year"]],
          value = log(sqrt(log(0.01^2 + 1)))
        ),
        by = c("fleet_name", "label", "time")
      )
    
    # Configure and Fit FIMS Model
    fit_fims_fixed_effects <- parameters |>
      # Initialize the FIMS model object with the updated parameters
      FIMS::initialize_fims(data = data_fims) |>
      # Run the TMB optimization (estimation)
      FIMS::fit_fims()

    fims_estimates <- FIMS::get_estimates(fit_fims_fixed_effects)
    # Define save paths
    output_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_fixed_effects.RDS")
    # Save the output
    saveRDS(fims_estimates, file = output_path_fixed_effects)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_fixed_effects <- FIMS::get_max_gradient(fit_fims_fixed_effects)
    # Define save paths
    max_gradient_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_fixed_effects.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_fixed_effects, file = max_gradient_path_fixed_effects)

    # Check hessian
    sdr <- FIMS::get_sdreport(fit_fims_fixed_effects)
    hessian_fixed_effects <- sdr[["pdHess"]]
    # Define save paths
    hessian_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_fixed_effects.RDS")
    # Save the hessian
    saveRDS(hessian_fixed_effects, file = hessian_path_fixed_effects)

    FIMS::clear()
    
    # random effects
    estimation_mode <- TRUE
    random_effects <- c(recruitment = "log_devs")
    map <- list()

    # Extract fishing fleet landings data (observed) and initialize index module
    landings <- em_input[["L.obs"]][["fleet1"]]

    # set fishing fleet landings data, need to set dimensions of data index
    # currently FIMS only has a fleet module that takes index for both survey index and fishery landings
    fishing_fleet_landings <- methods::new(Landings, om_input[["nyr"]])
    purrr::walk(
      1:om_input[["nyr"]],
      \(x) fishing_fleet_landings$landings_data$set(x - 1, landings[x])
    )

    # set fishing fleet age comp data, need to set dimensions of age comps
    # Here the new function initializes the object with length nyr*n_ages
    fishing_fleet_age_comp <- methods::new(AgeComp, om_input[["nyr"]], om_input[["nages"]])

    # Here we fill in the values for the object with the observed age comps for fleet one
    # we multiply these proportions by the sample size for likelihood weighting
    purrr::walk(
      1:(om_input[["nyr"]] * om_input[["nages"]]),
      \(x) fishing_fleet_age_comp$age_comp_data$set(
        x - 1,
        (c(t(em_input[["L.age.obs"]][["fleet1"]])) * em_input[["n.L"]][["fleet1"]])[x]
      )
    )

    # Fleet
    # Create the fishing fleet
    fishing_fleet_selectivity <- methods::new(LogisticSelectivity)
    fishing_fleet_selectivity$inflection_point[1]$value <- om_input[["sel_fleet"]][["fleet1"]][["A50.sel1"]]

    # turn on estimation of inflection_point
    fishing_fleet_selectivity$inflection_point[1]$estimation_type$set("fixed_effects")
    fishing_fleet_selectivity$slope[1]$value <- om_input[["sel_fleet"]][["fleet1"]][["slope.sel1"]]

    # turn on estimation of slope
    fishing_fleet_selectivity$slope[1]$estimation_type$set("fixed_effects")

    # Initialize the fishing fleet module
    fishing_fleet <- methods::new(Fleet)
    # Set number of years
    fishing_fleet$n_years$set(om_input[["nyr"]])
    # Set number of age classes
    fishing_fleet$n_ages$set(om_input[["nages"]])

    fishing_fleet$log_Fmort$resize(om_input[["nyr"]])
    for (y in 1:om_input$nyr) {
      # Log-transform OM fishing mortality
      fishing_fleet$log_Fmort[y]$value <- log(om_output[["f"]][y])
    }
    fishing_fleet$log_Fmort$set_all_estimable(TRUE)
    fishing_fleet$log_q[1]$value <- log(1.0)
    fishing_fleet$log_q[1]$estimation_type$set("constant")
    fishing_fleet$SetSelectivityID(fishing_fleet_selectivity$get_id())
    fishing_fleet$SetObservedLandingsDataID(fishing_fleet_landings$get_id())
    fishing_fleet$SetObservedAgeCompDataID(fishing_fleet_age_comp$get_id())

    # Set up fishery index data using the lognormal
    fishing_fleet_landings_distribution <- methods::new(DlnormDistribution)
    # lognormal observation error transformed on the log scale
    fishing_fleet_landings_distribution$log_sd$resize(om_input[["nyr"]])
    for (y in 1:om_input[["nyr"]]) {
      # Compute lognormal SD from OM coefficient of variation (CV)
      fishing_fleet_landings_distribution$log_sd[y]$value <- log(sqrt(log(em_input[["cv.L"]][["fleet1"]]^2 + 1)))
    }
    fishing_fleet_landings_distribution$log_sd$set_all_estimable(FALSE)
    # Set Data using the IDs from the modules defined above
    fishing_fleet_landings_distribution$set_observed_data(fishing_fleet$GetObservedLandingsDataID())
    fishing_fleet_landings_distribution$set_distribution_links("data", fishing_fleet$log_landings_expected$get_id())

    # Set up fishery age composition data using the multinomial
    fishing_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
    fishing_fleet_agecomp_distribution$set_observed_data(fishing_fleet$GetObservedAgeCompDataID())
    fishing_fleet_agecomp_distribution$set_distribution_links("data", fishing_fleet$agecomp_proportion$get_id())

    # Repeat similar setup for the survey fleet (e.g., index, age comp, and length comp)
    # This includes initializing logistic selectivity, observed data modules, and distribution links.
    survey_index <- em_input[["surveyB.obs"]][["survey1"]]
    survey_fleet_index <- methods::new(Index, om_input[["nyr"]])
    purrr::walk(
      1:om_input[["nyr"]],
      \(x) survey_fleet_index$index_data$set(x - 1, survey_index[x])
    )

    survey_fleet_age_comp <- methods::new(AgeComp, om_input[["nyr"]], om_input[["nages"]])
    purrr::walk(
      1:(om_input[["nyr"]] * om_input[["nages"]]),
      \(x) survey_fleet_age_comp$age_comp_data$set(
        x - 1,
        (c(t(em_input[["survey.age.obs"]][["survey1"]])) * em_input[["n.survey"]][["survey1"]])[x]
      )
    )

    # Fleet
    # Create the survey fleet
    survey_fleet_selectivity <- methods::new(LogisticSelectivity)
    survey_fleet_selectivity$inflection_point[1]$value <- om_input[["sel_survey"]][["survey1"]][["A50.sel1"]]

    # turn on estimation of inflection_point
    survey_fleet_selectivity$inflection_point[1]$estimation_type$set("fixed_effects")
    survey_fleet_selectivity$slope[1]$value <- om_input[["sel_survey"]][["survey1"]][["slope.sel1"]]

    # turn on estimation of slope
    survey_fleet_selectivity$slope[1]$estimation_type$set("fixed_effects")

    survey_fleet <- methods::new(Fleet)
    survey_fleet$n_ages$set(om_input[["nages"]])
    survey_fleet$n_years$set(om_input[["nyr"]])
    survey_fleet$log_Fmort$resize(om_input[["nyr"]])
    for (y in 1:om_input$nyr) {
      # Set very low survey fishing mortality
      survey_fleet$log_Fmort[y]$value <- -200
    }
    survey_fleet$log_Fmort$set_all_estimable(FALSE)
    survey_fleet$log_q[1]$value <- log(om_output[["survey_q"]][["survey1"]])
    survey_fleet$log_q[1]$estimation_type$set("fixed_effects")
    survey_fleet$SetSelectivityID(survey_fleet_selectivity$get_id())
    survey_fleet$SetObservedIndexDataID(survey_fleet_index$get_id())
    survey_fleet$SetObservedAgeCompDataID(survey_fleet_age_comp$get_id())

    # Set up survey index data using the lognormal
    survey_fleet_index_distribution <- methods::new(DlnormDistribution)

    # lognormal observation error transformed on the log scale
    # sd = sqrt(log(cv^2 + 1)), sd is log transformed
    survey_fleet_index_distribution$log_sd$resize(om_input[["nyr"]])
    for (y in 1:om_input$nyr) {
      survey_fleet_index_distribution$log_sd[y]$value <- log(sqrt(log(em_input[["cv.survey"]][["survey1"]]^2 + 1)))
    }
    survey_fleet_index_distribution$log_sd$set_all_estimable(FALSE)
    # Set Data using the IDs from the modules defined above
    survey_fleet_index_distribution$set_observed_data(survey_fleet$GetObservedIndexDataID())
    survey_fleet_index_distribution$set_distribution_links("data", survey_fleet$log_index_expected$get_id())

    # Age composition distribution
    survey_fleet_agecomp_distribution <- methods::new(DmultinomDistribution)
    survey_fleet_agecomp_distribution$set_observed_data(survey_fleet$GetObservedAgeCompDataID())
    survey_fleet_agecomp_distribution$set_distribution_links("data", survey_fleet$agecomp_proportion$get_id())

    # Recruitment
    # create new module in the recruitment class (specifically Beverton-Holt,
    # when there are other options, this would be where the option would be chosen)
    recruitment <- methods::new(BevertonHoltRecruitment)
    if (is.null(random_effects) || random_effects[["recruitment"]] == "log_devs") {
      recruitment_process <- new(LogDevsRecruitmentProcess)
    } else {
      recruitment_process <- new(LogRRecruitmentProcess)
    }
    recruitment$SetRecruitmentProcessID(recruitment_process$get_id())

    # NOTE: in first set of parameters below (for recruitment),
    # $estimation_type (default is "constant")
    # is defined even if it matches the defaults in order to provide an example
    # of how that is done. Other sections of the code below leave defaults in
    # place as appropriate.

    # set up log_rzero (equilibrium recruitment)
    recruitment$log_rzero[1]$value <- log(om_input[["R0"]])
    recruitment$log_rzero[1]$estimation_type$set("fixed_effects")
    # set up logit_steep
    recruitment$logit_steep[1]$value <- -log(1.0 - om_input[["h"]]) + log(om_input[["h"]] - 0.2)
    recruitment$logit_steep[1]$estimation_type$set("constant")
    recruitment$n_years$set(om_input[["nyr"]])

    # turn on estimation of deviations
    # recruit deviations should enter the model in normal space.
    # The log is taken in the likelihood calculations
    # alternative setting: recruitment$log_devs <- rep(0, length(om_input$logR.resid))


    if (is.null(random_effects) || random_effects[["recruitment"]] == "log_devs") {
      recruitment$log_devs$resize(om_input[["nyr"]] - 1)
      for (y in 1:(om_input[["nyr"]] - 1)) {
        recruitment$log_devs[y]$value <- om_input[["logR.resid"]][y + 1]
      }
    }
    if ("recruitment" %in% names(random_effects)) {
      if (random_effects[["recruitment"]] == "log_devs") {
        recruitment$log_devs$set_all_random(TRUE)
      }
      if (random_effects[["recruitment"]] == "log_r") {
        recruitment$log_r$resize(om_input[["nyr"]] - 1)
        for (y in 1:(om_input[["nyr"]] - 1)) {
          recruitment$log_r[y]$value <- 1
        }
        recruitment$log_r$set_all_random(TRUE)
      }
    }
    if (is.null(random_effects)) {
      # TODO: integration tests fail after setting recruitment log_devs all estimable.
      # We need to debug the issue, then uncomment the line below.
      recruitment$log_devs$set_all_estimable(TRUE)
    }

    if ("selectivity" %in% names(random_effects)) {
      if (random_effects[["selectivity"]] == "log_devs") {
        fishing_fleet_selectivity$log_devs$set_all_random(TRUE)
        survey_fleet_selectivity$log_devs$set_all_random(TRUE)
      }
      if (random_effects[["selectivity"]] == "log_sel") {
        fishing_fleet_selectivity$log_sel$set_all_random(TRUE)
        survey_fleet_selectivity$log_sel$set_all_random(TRUE)
      }
      if (random_effects[["selectivity"]] == "pars") {
        fishing_fleet_selectivity$inflection_point$estimation_type$set("random_effects")
        fishing_fleet_selectivity$inflection_point$slope <- "random_effects"
        survey_fleet_selectivity$inflection_point$estimation_type$set("random_effects")
        survey_fleet_selectivity$inflection_point$slope <- "random_effects"
      }
    }
    recruitment_distribution <- methods::new(DnormDistribution)
    # set up logR_sd using the normal log_sd parameter
    # logR_sd is NOT logged. It needs to enter the model logged b/c the exp() is
    # taken before the likelihood calculation
    recruitment_distribution$log_sd$resize(1)
    recruitment_distribution$log_sd[1]$value <- log(om_input[["logR_sd"]])
    recruitment_distribution$x$resize(om_input[["nyr"]] - 1)
    recruitment_distribution$expected_values$resize(om_input[["nyr"]] - 1)
    for (i in 1:(om_input[["nyr"]] - 1)) {
      recruitment_distribution$x[i]$value <- 0
      recruitment_distribution$expected_values[i]$value <- 0
    }
    if ("recruitment" %in% names(random_effects)) {
      if (random_effects[["recruitment"]] == "log_devs") {
        recruitment_distribution$log_sd[1]$estimation_type$set("fixed_effects")
        recruitment_distribution$set_distribution_links("random_effects", recruitment$log_devs$get_id())
      }
      if (random_effects[["recruitment"]] == "log_r") {
        recruitment_distribution$log_sd[1]$value <- log(1)
        recruitment_distribution$log_sd[1]$estimation_type$set("fixed_effects")
        recruitment_distribution$set_distribution_links("random_effects", c(recruitment$log_r$get_id(), recruitment$log_expected_recruitment$get_id()))
      }
    }

    if (is.null(random_effects)) {
      recruitment_distribution$set_distribution_links("random_effects", recruitment$log_devs$get_id())
    }

    # Growth
    ewaa_growth <- methods::new(EWAAGrowth)
    ewaa_growth$ages$resize(om_input[["nages"]])
    purrr::walk(
      seq_along(om_input[["ages"]]),
      \(x) ewaa_growth$ages$set(x - 1, om_input[["ages"]][x])
    )
    ewaa_growth$weights$resize(om_input[["nages"]])
    purrr::walk(
      seq_along(om_input[["W.mt"]]),
      \(x) ewaa_growth$weights$set(x - 1, om_input[["W.mt"]][x])
    )

    # Maturity
    maturity <- methods::new(LogisticMaturity)
    maturity$inflection_point[1]$value <- om_input[["A50.mat"]]
    maturity$inflection_point[1]$estimation_type$set("constant")
    maturity$slope[1]$value <- om_input[["slope.mat"]]
    maturity$slope[1]$estimation_type$set("constant")

    # Population
    population <- methods::new(Population)
    population$log_M$resize(om_input[["nyr"]] * om_input[["nages"]])
    for (i in 1:(om_input[["nyr"]] * om_input[["nages"]])) {
      population$log_M[i]$value <- log(om_input[["M.age"]][1])
    }
    population$log_M$set_all_estimable(FALSE)
    population$log_init_naa$resize(om_input[["nages"]])
    for (i in 1:om_input$nages) {
      population$log_init_naa[i]$value <- log(om_output[["N.age"]][1, i])
    }
    population$log_init_naa$set_all_estimable(TRUE)
    population$n_ages$set(om_input[["nages"]])
    population$ages$resize(om_input[["nages"]])
    purrr::walk(
      seq_along(om_input[["ages"]]),
      \(x) population$ages$set(x - 1, om_input[["ages"]][x])
    )
    population$n_fleets$set(sum(om_input[["fleet_num"]], om_input[["survey_num"]]))
    population$n_years$set(om_input[["nyr"]])
    population$SetRecruitmentID(recruitment$get_id())
    population$SetGrowthID(ewaa_growth$get_id())
    population$SetMaturityID(maturity$get_id())
    population$AddFleet(fishing_fleet$get_id())
    population$AddFleet(survey_fleet$get_id())

    # Set up catch at age model
    caa <- methods::new(CatchAtAge)
    caa$AddPopulation(population$get_id())

    # Set-up TMB
    CreateTMBModel()
    # Create parameter list from Rcpp modules
    parameters <- list(
      p = get_fixed(),
      re = get_random()
    )
    obj <- TMB::MakeADFun(
      data = list(), parameters, DLL = "FIMS",
      silent = TRUE, map = map, random = "re"
    )

    # Optimization with nlminb
    opt <- NULL
    random_effects_number_of_loops <- 3
    if (estimation_mode == TRUE) {
      control <- list(eval.max = 10000, iter.max = 10000, trace = 0)
      opt <- stats::nlminb(
        start = obj[["par"]],
        objective = obj[["fn"]],
        gradient = obj[["gr"]],
        control = control
      )

      maxgrad0 <- max(abs(obj$gr(opt$par)))
      maxgrad <- maxgrad0
      if (random_effects_number_of_loops > 0) {
        for (ii in seq_len(random_effects_number_of_loops)) {
          opt <- stats::nlminb(
            start = opt[["par"]],
            objective = obj[["fn"]],
            gradient = obj[["gr"]],
            control = control
          )
          maxgrad <- max(abs(obj$gr(opt$par)))
        }
      }
      FIMS::set_fixed(opt$par)
      fims_finalized <- caa$get_output(do_sd_report = estimation_mode)
    }

    # Call report using MLE parameter values, or
    # the input values if optimization is skipped
    report <- obj[["report"]](obj[["env"]][["last.par.best"]])

    sdr <- TMB::sdreport(obj)
    sdr_report <- summary(sdr, "report")
    sdr_fixed <- summary(sdr, "fixed")
    sdr_random <- summary(sdr, "random")
    row.names(sdr_fixed) <- names(FIMS:::get_parameter_names(sdr_fixed[, 1]))
    hessian <- sdr[["pdHess"]]
    
    fit_fims_random_effects <- list(
      sdr_report = sdr_report,
      sdr_fixed = sdr_fixed,
      sdr_random = sdr_random,
      hessian = hessian
    )

    # Define save paths
    output_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_random_effects.RDS")
    # Save the output
    saveRDS(fit_fims_random_effects, file = output_path_random_effects)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_random_effects <- max(abs(sdr[["gradient.fixed"]]))
    # Define save paths
    max_gradient_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_random_effects.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_random_effects, file = max_gradient_path_random_effects)

    # Clear FIMS before the next simulation
    FIMS::clear()
  }
}

#' Prepare data for FIMS
#'
#' @description
#' A helper function to format OM and EM data into the specific data.frame
#' structure required by `FIMS::FIMSFrame`.
#'
#' @param om_input A list containing OM input parameters (e.g., years, weights,
#'   ages).
#' @param om_output A list containing OM output data (e.g., landings, survey
#'   index).
#' @param em_input A list containing EM input data (e.g., *observed* landings,
#'   *observed* survey index, and their CVs).
#'
#' @return A `data.frame` formatted for input into `FIMS::FIMSFrame`.
#' @noRd
prepare_data_fims <- function(om_input, om_output, em_input) {
  # Format observed landings data
  landings_data <- data.frame(
    type = "landings",
    name = names(om_output[["L.mt"]])[1],
    age = NA,
    timing = om_input[["year"]],
    value = em_input[["L.obs"]][[1]],
    unit = "mt",
    uncertainty = cv_2_sd(em_input[["cv.L"]][[1]])
  )
  # Format observed survey index data
  index_data <- data.frame(
    type = "index",
    name = names(om_output[["survey_index"]])[1],
    age = NA,
    timing = om_input[["year"]],
    value = em_input[["surveyB.obs"]][[1]],
    unit = "mt",
    uncertainty = cv_2_sd(em_input[["cv.survey"]][[1]])
  )
  # Format age-composition data
  age_data <- rbind(
    data.frame(
      name = names(em_input[["n.L"]]),
      em_input[["L.age.obs"]][["fleet1"]],
      unit = "proportion",
      uncertainty = em_input[["n.L"]][["fleet1"]],
      timing = om_input[["year"]]
    ),
    data.frame(
      name = names(om_output[["survey_age_comp"]])[1],
      em_input[["survey.age.obs"]][[1]],
      unit = "proportion",
      uncertainty = om_input[["n.survey"]][["survey1"]],
      timing = om_input[["year"]]
    )
  ) |>
    dplyr::mutate(
      type = "age_comp"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_prefix = "X",
      names_to = "age",
      values_to = "value",
      # Convert the "age" column from strings to integers
      names_transform = list(age = as.integer)
    )
  # Create a data frame of all years
  timingfishery <- data.frame(
    timing = om_input[["year"]]
  )
  # Create a data frame of weights-at-age (assumed constant over time)
  weights_fishery <- data.frame(
    type = "weight-at-age",
    name = names(em_input[["n.L"]]),
    age = seq_along(om_input[["W.kg"]]),
    value = om_input[["W.mt"]],
    uncertainty = NA,
    unit = "mt"
  )
  # Expand weight-at-age to all years by merging
  weight_at_age_data <- merge(timingfishery, weights_fishery)
  # Combine all data components into one data.frame
  data_fims <- rbind(landings_data, index_data, age_data, weight_at_age_data) |>
    dplyr::mutate(
      length = NA,
      .after = "age"
    )
}

#' Convert CV to Lognormal Standard Deviation
#'
#' @description
#' Calculates the standard deviation ($\sigma$) for a lognormal distribution
#' based on the arithmetic Coefficient of Variation (CV).
#'
#' @details
#' The formula used is: $\sigma = \sqrt{\log(CV^2 + 1)}$. This is the
#' standard conversion used when assuming a lognormal error structure
#' for indices or catch data.
#'
#' @param x A numeric vector of Coefficient(s) of Variation (CV).
#'
#' @return A numeric vector of the corresponding lognormal standard
#'   deviations ($\sigma$).
#' @noRd
cv_2_sd <- function(x) {
  sqrt(log(x^2 + 1))
}
