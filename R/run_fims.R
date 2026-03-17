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
    fit_fims_random_effects <- parameters |>
      # Initialize the FIMS model object with the updated parameters
      FIMS::initialize_fims(data = data_fims) |>
      # Run the TMB optimization (estimation)
      FIMS::fit_fims()

    fims_estimates_random_effects <- FIMS::get_estimates(fit_fims_random_effects)
    # Define save paths
    output_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_random_effects.RDS")
    # Save the output
    saveRDS(fims_estimates_random_effects, file = output_path_random_effects)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_random_effects <- FIMS::get_max_gradient(fit_fims_random_effects)
    # Define save paths
    max_gradient_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_random_effects.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_random_effects, file = max_gradient_path_random_effects)

    # Check hessian
    obj_random_effects <- FIMS::get_obj(fit_fims_random_effects)
    hessian_random_effects <- obj_random_effects$hessian
    # Define save paths
    hessian_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_random_effects.RDS")
    # Save the hessian
    saveRDS(hessian_random_effects, file = hessian_path_random_effects)

    FIMS::clear()

    # Random effects with sigmaR estimated
    parameters_sigmaR_estimated <- parameters |>
      # Update log_sd for log_devs in the Recruitment module to be estimated
      dplyr::rows_update(
        tibble::tibble(
          module_name = "Recruitment",
          label = "log_sd",
          value = om_input[["logR_sd"]],
          estimation_type = "fixed_effects"
        ),
        by = c("module_name", "label")
      )
    
    # Configure and Fit FIMS Model
    fit_fims_random_effects_sigmaR_estimated <- parameters_sigmaR_estimated |>
      # Initialize the FIMS model object with the updated parameters
      FIMS::initialize_fims(data = data_fims) |>
      # Run the TMB optimization (estimation)
      FIMS::fit_fims()

    fims_estimates_random_effects_sigmaR_estimated <- FIMS::get_estimates(fit_fims_random_effects_sigmaR_estimated)
    # Define save paths
    output_path_random_effects_sigmaR_estimated <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_random_effects_sigmaR_estimated.RDS")
    # Save the output
    saveRDS(fims_estimates_random_effects_sigmaR_estimated, file = output_path_random_effects_sigmaR_estimated)

    # Check convergence by extracting the maximum gradient
    max_gradient_fims_random_effects_sigmaR_estimated <- FIMS::get_max_gradient(fit_fims_random_effects_sigmaR_estimated)
    # Define save paths
    max_gradient_path_random_effects_sigmaR_estimated <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "max_gradient_fims_random_effects_sigmaR_estimated.RDS")
    # Save the max gradient
    saveRDS(max_gradient_fims_random_effects_sigmaR_estimated, file = max_gradient_path_random_effects_sigmaR_estimated)

    # Check hessian
    obj_random_effects_sigmaR_estimated <- FIMS::get_obj(fit_fims_random_effects_sigmaR_estimated)
    hessian_random_effects_sigmaR_estimated <- obj_random_effects_sigmaR_estimated$hessian
    # Define save paths
    hessian_path_random_effects_sigmaR_estimated <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "hessian_fims_random_effects_sigmaR_estimated.RDS")
    # Save the hessian
    saveRDS(hessian_random_effects_sigmaR_estimated, file = hessian_path_random_effects_sigmaR_estimated)

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
    fit_fims_fixed_effects <- parameters_fixed_effects |>
      # Initialize the FIMS model object with the updated parameters
      FIMS::initialize_fims(data = data_fims) |>
      # Run the TMB optimization (estimation)
      FIMS::fit_fims()

    fims_estimates_fixed_effects <- FIMS::get_estimates(fit_fims_fixed_effects)
    # Define save paths
    output_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_fims_fixed_effects.RDS")
    # Save the output
    saveRDS(fims_estimates_fixed_effects, file = output_path_fixed_effects)

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
