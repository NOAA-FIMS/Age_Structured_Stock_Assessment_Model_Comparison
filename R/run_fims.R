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

  cl <- ifelse(detectCores()==1, detectCores(), detectCores()-2)
  registerDoParallel(cl)

  foreach (om_sim = 1:om_sim_num) %dopar% {
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
          value = om_input[["logR.resid"]][-1]
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
    fims_elapsed_random_effects <- system.time({
      fit_fims_random_effects <- parameters |>
        # Initialize the FIMS model object with the updated parameters
        FIMS::initialize_fims(data = data_fims) |>
        # Run the TMB optimization (estimation)
        FIMS::fit_fims()
    })

    run_time_random_effects <- FIMS::get_timing(fit_fims_random_effects) |> 
      as.numeric() |>
      setNames(c("fit_optimization", "fit_sdreport", "fit_total")) |>
      c(total = fims_elapsed_random_effects[["elapsed"]])
    run_time_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "run_time_fims_random_effects.RDS")
    saveRDS(run_time_random_effects, file = run_time_path_random_effects)

    fims_estimates_random_effects <- FIMS::get_estimates(fit_fims_random_effects)
    # Define save paths
    output_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_random_effects.RDS")
    # Save the output
    saveRDS(fims_estimates_random_effects, file = output_path_random_effects)

    # Check optimizer convergence code
    # 0 = converged, >0 = not converged
    optimizer_convergence_random_effects <- FIMS::get_opt(fit_fims_random_effects)[["convergence"]]
    optimizer_convergence_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "optimizer_convergence_fims_random_effects.RDS")
    saveRDS(optimizer_convergence_random_effects, file = optimizer_convergence_path_random_effects)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_random_effects <- FIMS::get_max_gradient(fit_fims_random_effects)
    # Define save paths
    max_gradient_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_random_effects.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_random_effects, file = max_gradient_path_random_effects)

    # Check hessian and NA SEs
    # TRUE/FALSE: is Hessian positive definite?
    sdreport_random_effects <- FIMS::get_sdreport(fit_fims_random_effects)
    if (!is.null(sdreport_random_effects)) {
      hessian_random_effects <- sdreport_random_effects[["pdHess"]]
      # Define save paths
      hessian_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_random_effects.RDS")
      # Save the hessian
      saveRDS(hessian_random_effects, file = hessian_path_random_effects)

      na_count_random_effects <- count_na_standard_errors(sdreport_random_effects)
      na_count_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "na_count_random_effects.RDS")
      saveRDS(na_count_random_effects, file = na_count_path_random_effects)

      condition_number_random_effects <- get_condition_number(
        FIMS::get_obj(fit_fims_random_effects),
        FIMS::get_opt(fit_fims_random_effects),
        sdreport_random_effects
      )
      condition_number_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "condition_number_random_effects.RDS")
      saveRDS(condition_number_random_effects, file = condition_number_path_random_effects)
    }
    
    FIMS::clear()

    # Random effects with constant sigmaR
    parameters_sigmaR_constant <- parameters |>
      # Update log_sd for log_devs in the Recruitment module to be estimated
      dplyr::rows_update(
        tibble::tibble(
          module_name = "Recruitment",
          label = "log_sd",
          value = om_input[["logR_sd"]],
          estimation_type = "constant"
        ),
        by = c("module_name", "label")
      )
    
    # Configure and Fit FIMS Model
    fims_elapsed_random_effects_sigmaR_constant <- system.time({
      fit_fims_random_effects_sigmaR_constant <- parameters_sigmaR_constant |>
        # Initialize the FIMS model object with the updated parameters
        FIMS::initialize_fims(data = data_fims) |>
        # Run the TMB optimization (estimation)
        FIMS::fit_fims()
    })
       
    run_time_random_effects_sigmaR_constant <- FIMS::get_timing(fit_fims_random_effects_sigmaR_constant) |> 
      as.numeric() |>
      setNames(c("fit_optimization", "fit_sdreport", "fit_total")) |>
      c(total = fims_elapsed_random_effects_sigmaR_constant[["elapsed"]])
    run_time_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "run_time_fims_random_effects_sigmaR_constant.RDS")
    saveRDS(run_time_random_effects_sigmaR_constant, file = run_time_path_random_effects_sigmaR_constant)

    fims_estimates_random_effects_sigmaR_constant <- FIMS::get_estimates(fit_fims_random_effects_sigmaR_constant)
    # Define save paths
    output_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_random_effects_sigmaR_constant.RDS")
    # Save the output
    saveRDS(fims_estimates_random_effects_sigmaR_constant, file = output_path_random_effects_sigmaR_constant)

    # Check optimizer convergence code
    # 0 = converged, >0 = not converged
    optimizer_convergence_random_effects_sigmaR_constant <- FIMS::get_opt(fit_fims_random_effects_sigmaR_constant)[["convergence"]]
    optimizer_convergence_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "optimizer_convergence_fims_random_effects_sigmaR_constant.RDS")
    saveRDS(optimizer_convergence_random_effects_sigmaR_constant, file = optimizer_convergence_path_random_effects_sigmaR_constant)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_random_effects_sigmaR_constant <- FIMS::get_max_gradient(fit_fims_random_effects_sigmaR_constant)
    # Define save paths
    max_gradient_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_random_effects_sigmaR_constant.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_random_effects_sigmaR_constant, file = max_gradient_path_random_effects_sigmaR_constant)
    
    # Check hessian and NA SEs
    # TRUE/FALSE: is Hessian positive definite?
    sdreport_random_effects_sigmaR_constant <- FIMS::get_sdreport(fit_fims_random_effects_sigmaR_constant)
    if (!is.null(sdreport_random_effects_sigmaR_constant)) {
      hessian_random_effects_sigmaR_constant <- sdreport_random_effects_sigmaR_constant[["pdHess"]]
      # Define save paths
      hessian_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_random_effects_sigmaR_constant.RDS")
      # Save the hessian
      saveRDS(hessian_random_effects_sigmaR_constant, file = hessian_path_random_effects_sigmaR_constant)

      na_count_random_effects_sigmaR_constant <- count_na_standard_errors(sdreport_random_effects_sigmaR_constant)
      na_count_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "na_count_random_effects_sigmaR_constant.RDS")
      saveRDS(na_count_random_effects_sigmaR_constant, file = na_count_path_random_effects_sigmaR_constant)

      condition_number_random_effects_sigmaR_constant <- get_condition_number(
        FIMS::get_obj(fit_fims_random_effects_sigmaR_constant),
        FIMS::get_opt(fit_fims_random_effects_sigmaR_constant),
        sdreport_random_effects_sigmaR_constant
      )
      condition_number_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "condition_number_random_effects_sigmaR_constant.RDS")
      saveRDS(condition_number_random_effects_sigmaR_constant, file = condition_number_path_random_effects_sigmaR_constant)
    }

    FIMS::clear()
    
    # fixed effects
    parameters_fixed_effects <- parameters |>
      # Update log_sd for log_devs in the Recruitment module to be estimated
      dplyr::rows_update(
        tibble::tibble(
          module_name = "Recruitment",
          label = "log_sd",
          value = om_input[["logR_sd"]],
          estimation_type = "constant"
        ),
        by = c("module_name", "label")
      ) |>
      # Update log_devs in the Recruitment module to be estimated as fixed effects
      dplyr::rows_update(
        tibble::tibble(
          label = "log_devs",
          time = om_input[["year"]][-1],
          value = om_input[["logR.resid"]][-1],
          estimation_type = "fixed_effects"
        ),
        by = c("label", "time")
      )
    
    # Configure and Fit FIMS Model
    fims_elapsed_fixed_effects <- system.time({
      fit_fims_fixed_effects <- parameters_fixed_effects |>
        # Initialize the FIMS model object with the updated parameters
        FIMS::initialize_fims(data = data_fims) |>
        # Run the TMB optimization (estimation)
        FIMS::fit_fims()
    })
    
    run_time_fixed_effects <- FIMS::get_timing(fit_fims_fixed_effects) |> 
      as.numeric() |>
      setNames(c("fit_optimization", "fit_sdreport", "fit_total")) |>
      c(total = fims_elapsed_fixed_effects[["elapsed"]])
    run_time_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "run_time_fims_fixed_effects.RDS")
    saveRDS(run_time_fixed_effects, file = run_time_path_fixed_effects)

    fims_estimates_fixed_effects <- FIMS::get_estimates(fit_fims_fixed_effects)
    # Define save paths
    output_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_fixed_effects.RDS")
    # Save the output
    saveRDS(fims_estimates_fixed_effects, file = output_path_fixed_effects)

    # Check optimizer convergence code
    # 0 = converged, >0 = not converged
    optimizer_convergence_fixed_effects <- FIMS::get_opt(fit_fims_fixed_effects)[["convergence"]]
    optimizer_convergence_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "optimizer_convergence_fims_fixed_effects.RDS")
    saveRDS(optimizer_convergence_fixed_effects, file = optimizer_convergence_path_fixed_effects)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_fixed_effects <- FIMS::get_max_gradient(fit_fims_fixed_effects)
    # Define save paths
    max_gradient_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_fixed_effects.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_fixed_effects, file = max_gradient_path_fixed_effects)

    sdreport_fixed_effects <- FIMS::get_sdreport(fit_fims_fixed_effects)
    if (!is.null(sdreport_fixed_effects)) {
      hessian_fixed_effects <- sdreport_fixed_effects[["pdHess"]]
      # Define save paths
      hessian_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_fixed_effects.RDS")
      # Save the hessian
      saveRDS(hessian_fixed_effects, file = hessian_path_fixed_effects)

      na_count_fixed_effects <- count_na_standard_errors(sdreport_fixed_effects)
      na_count_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "na_count_fixed_effects.RDS")
      saveRDS(na_count_fixed_effects, file = na_count_path_fixed_effects)

      condition_number_fixed_effects <- get_condition_number(
        FIMS::get_obj(fit_fims_fixed_effects),
        FIMS::get_opt(fit_fims_fixed_effects),
        sdreport_fixed_effects
      )
      condition_number_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "condition_number_fixed_effects.RDS")
      saveRDS(condition_number_fixed_effects, file = condition_number_path_fixed_effects)
    }

    FIMS::clear()
  }
  # stopCluster(cl)

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
  timing_fishery <- data.frame(
    timing = om_input[["year"]]
  )
  # Create a data frame of weights-at-age (assumed constant over time)
  weights_fishery <- data.frame(
    type = "weight_at_age",
    name = names(em_input[["n.L"]]),
    age = seq_along(om_input[["W.kg"]]),
    value = om_input[["W.mt"]],
    uncertainty = NA,
    unit = "mt"
  )
  # Expand weight-at-age to all years by merging
  weight_at_age_data <- merge(
    # Add on one more year for weight_at_age data because the model needs to
    # compute spawning biomass for the beginning of the year following the
    # terminal year
    dplyr::bind_rows(
      timing_fishery,
      data.frame(timing = max(timing_fishery[["timing"]]) + 1)
    ),
    weights_fishery
  )
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

#' Count NA standard errors across all parameter types
count_na_standard_errors <- function(sdreport) {
  total_na <- 0
  
  # Fixed effects
  fixed_summary <- summary(sdreport, "fixed")
  if (!is.null(fixed_summary) && nrow(fixed_summary) > 0) {
    total_na <- total_na + sum(is.na(fixed_summary[, "Std. Error"]))
  }
  
  # Random effects
  random_summary <- summary(sdreport, "random")
  if (!is.null(random_summary) && nrow(random_summary) > 0) {
    total_na <- total_na + sum(is.na(random_summary[, "Std. Error"]))
  }
  
  # Derived quantities
  report_summary <- summary(sdreport, "report")
  if (!is.null(report_summary) && nrow(report_summary) > 0) {
    total_na <- total_na + sum(is.na(report_summary[, "Std. Error"]))
  }
  
  return(total_na)
}

#' Get condition number of Hessian
get_condition_number <- function(obj, opt, sdreport) {
  if (length(obj[["env"]][["random"]]) > 0) {
    hessian <- obj[["env"]]$spHess(random = TRUE)
  } else {
    hessian <- as.matrix(obj$he(opt[["par"]]))
  }
  kappa(hessian)
}
