#' Run the WHAM Estimation Model (EM)
#'
#' @description
#' This function iterates through a specified number of Operating Model (OM)
#' simulations, prepares the data, fits the WHAM model using TMB, and saves
#' the outputs to a simulation-specific subdirectory.
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
run_wham <- function(
  maindir = maindir,
  subdir = "WHAM",
  om_sim_num = NULL,
  casedir = casedir,
  em_bias_cor = em_bias_cor
) {
  # Check if WHAM is installed
  if (!("wham" %in% installed.packages()[, "Package"])) devtools::install_github("timjmiller/wham", dependencies=TRUE)

  # Clean up previous WHAM outputs from the target directory to prevent conflicts
  unlink(list.files(file.path(casedir, "output", "WHAM"), full.names = TRUE), recursive = TRUE)
  # Create output directories for each simulation (e.g., .../WHAM/s1, .../WHAM/s2)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep = ""))))

  for (om_sim in 1:om_sim_num) {
    print(om_sim)
    # Load OM data
    load(file = file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep = "")))
    # Load the specific ASAP input data, the output of read_asap_dat() should then passed to 
    # prepare_wham_input().
    input_asap <- wham::read_asap3_dat(
      file.path(casedir, "output", "ASAP", paste("s", om_sim, sep=""), "asap3.DAT")
    )
    
    input_wham <- wham::prepare_wham_input(
      input_asap,
      recruit_model = 3,
      NAA_re = list(sigma="rec")
    )
    
    # Use true sigma
    rec_cv <- input_asap[[1]][["dat"]][["recruit_cv"]]
    rec_sigma <- sqrt(log(1 + rec_cv^2))
    n_ages <- input_wham[["data"]][["n_ages"]]
    input_wham[["par"]][["log_NAA_sigma"]][1, 1, 1] <- log(rec_sigma)[1]

    h_om <- om_input[["h"]]
    spr0_om <- om_input[["Phi.0"]] * 1000
    alpha_om <- (4 * h_om) / (spr0_om * (1 - h_om))
    input_wham[["par"]][["mean_rec_pars"]][1] <- log(alpha_om)
    input_wham[["map"]][["mean_rec_pars"]] <- factor(c(NA, 1))

    fit_wham_random_effects <- wham::fit_wham(input_wham, do.osa = F, do.retro = F) 

    # Extract runtime
    runtime_random_effects <- as.numeric(fit_wham_random_effects[["runtime"]])
    runtime_path_random_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "runtime_random_effects.RDS")
    saveRDS(runtime_random_effects, file = runtime_path_random_effects)
    
    # Define save paths
    output_path <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_wham_random_effects.RDS")
    # Save the output
    saveRDS(fit_wham_random_effects, file = output_path)

    # Check convergence by extracting the maximum gradient
    convergence_wham_random_effects <- fit_wham_random_effects$opt$convergence
    # Define save paths
    convergence_path <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "convergence_wham_random_effects.RDS")
    # Save the max gradient
    saveRDS(convergence_wham_random_effects, file = convergence_path)

    # Map log_NAA_sigma off so it's not estimated
    # TMB expects a factor mapping; setting to NA fixes it
    input_wham[["map"]][["log_NAA_sigma"]] <- rep(factor(NA), n_ages)
    fit_wham_random_effects_sigmaR_constant <- wham::fit_wham(input_wham, do.osa = F, do.retro = F) 

    # Extract runtime
    runtime_random_effects_sigmaR_constant <- as.numeric(fit_wham_random_effects_sigmaR_constant[["runtime"]])
    runtime_path_random_effects_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "runtime_random_effects_sigmaR_constant.RDS")
    saveRDS(runtime_random_effects_sigmaR_constant, file = runtime_path_random_effects_sigmaR_constant)
    
    # Define save paths
    output_path_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_wham_random_effects_sigmaR_constant.RDS")
    # Save the output
    saveRDS(fit_wham_random_effects_sigmaR_constant, file = output_path_sigmaR_constant)

    # Check convergence by extracting the maximum gradient
    convergence_wham_random_effects_sigmaR_constant <- fit_wham_random_effects_sigmaR_constant$opt$convergence
    # Define save paths
    convergence_path_sigmaR_constant <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "convergence_wham_random_effects_sigmaR_constant.RDS")
    # Save the max gradient
    saveRDS(convergence_wham_random_effects_sigmaR_constant, file = convergence_path_sigmaR_constant)

    # Set log_NAA to fixed effects 
    input_wham[["random"]] <- input_wham[["random"]][!grepl("log_NAA", input_wham[["random"]])]

    fit_wham_fixed_effects_logNAA <- wham::fit_wham(input_wham, do.osa = F, do.retro = F) 

    # Extract runtime
    runtime_fixed_effects_logNAA <- as.numeric(fit_wham_fixed_effects_logNAA[["runtime"]])
    runtime_path_fixed_effects_logNAA <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "runtime_fixed_effects_logNAA.RDS")
    saveRDS(runtime_fixed_effects_logNAA, file = runtime_path_fixed_effects_logNAA)
    
    # Define save paths
    output_path_fixed_effects_logNAA <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_wham_fixed_effects_logNAA.RDS")
    # Save the output
    saveRDS(fit_wham_fixed_effects_logNAA, file = output_path_fixed_effects_logNAA)

    # Check convergence by extracting the maximum gradient
    convergence_wham_fixed_effects_logNAA <- fit_wham_fixed_effects_logNAA$opt$convergence
    # Define save paths
    convergence_path_fixed_effects_logNAA <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "convergence_wham_fixed_effects_logNAA.RDS")
    # Save the max gradient
    saveRDS(convergence_wham_fixed_effects_logNAA, file = convergence_path_fixed_effects_logNAA)

    input_wham <- wham::prepare_wham_input(
      input_asap,
      recruit_model = 3,
      NAA_re = NULL
    )

    input_wham[["par"]][["mean_rec_pars"]][1] <- log(alpha_om)
    input_wham[["map"]][["mean_rec_pars"]] <- factor(c(NA, 1))

    fit_wham_fixed_effects <- wham::fit_wham(input_wham, do.osa = F, do.retro = F) 

    # Extract runtime
    runtime_fixed_effects <- as.numeric(fit_wham_fixed_effects[["runtime"]])
    runtime_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "runtime_fixed_effects.RDS")
    saveRDS(runtime_fixed_effects, file = runtime_path_fixed_effects)
    
    # Define save paths
    output_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "fit_wham_fixed_effects.RDS")
    # Save the output
    saveRDS(fit_wham_fixed_effects, file = output_path_fixed_effects)

    # Check convergence by extracting the maximum gradient
    convergence_wham_fixed_effects <- fit_wham_fixed_effects$opt$convergence
    # Define save paths
    convergence_path_fixed_effects <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), "convergence_wham_fixed_effects.RDS")
    # Save the max gradient
    saveRDS(convergence_wham_fixed_effects, file = convergence_path_fixed_effects)
  }
}
