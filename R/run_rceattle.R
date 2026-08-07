# =============================================================================
# run_rceattle() -- Rceattle estimation model for the ASSAMC OM-EM comparison.
#
# Analogue of run_fims() / run_wham(). Same contract:
#   * run_rceattle(maindir, subdir, om_sim_num, casedir, em_bias_cor, ...)
#   * loop over 1..om_sim_num replicates in a parallel foreach
#   * each worker load()s casedir/output/OM/OM{i}.RData -> om_input, om_output,
#     em_input
#   * fit three recruitment scenarios and saveRDS() a fixed set of result files
#     into casedir/output/<subdir>/s{i}/
#   * no return value
#
# The three scenarios mirror FIMS/WHAM. All fit a Beverton-Holt curve with
# steepness pinned at the OM's true value and the initial age structure
# estimated as free parameters -- the same treatment WHAM and FIMS get, so the
# comparison measures the platform rather than the configuration. They differ
# only in how the recruitment deviations are treated:
#
#   random_effects                  rec_dev random effects, sigmaR estimated
#   random_effects_sigmaR_constant  rec_dev random effects, sigmaR fixed
#   fixed_effects                   rec_dev penalised fixed effects, sigmaR fixed
#
# HELPER NAMES. run_fims.R defines cv_2_sd(), count_na_standard_errors() and
# get_condition_number() at package scope. DESCRIPTION has no Collate: field, so
# R collates R/ alphabetically and this file would shadow them -- and
# run_fims.R's get_condition_number() takes three arguments where ours takes one.
# The rceattle_ prefixes below keep them distinct; leave them in place. cv_2_sd()
# is reused rather than redefined, so observation error is specified the same way
# across estimation models.
#
# Requires: Rceattle, foreach, doParallel, parallel.
# =============================================================================

utils::globalVariables(c("om_input", "om_output", "em_input", "om_sim"))

# Scenario identifiers, reused for the file names.
RCEATTLE_SCENARIOS <- c(
  "random_effects",
  "random_effects_sigmaR_constant",
  "fixed_effects"
)


#' Translate one OM replicate into an Rceattle data_list
#'
#' The Rceattle analogue of prepare_data_fims(). The OM hands each estimation
#' model three lists via load("OM{i}.RData"):
#'
#'   om_input   true inputs / spec  (year, ages, M.age, W.mt, mat.age, sel_*, ...)
#'   om_output  true dynamics       (SSB, N.age, f, survey_q, ...) -- truth, for
#'                                  self-tests; NOT fed to the EM
#'   em_input   observed-with-error (L.obs, survey.obs, L.age.obs,
#'                                  survey.age.obs, n.L, n.survey, cv.L,
#'                                  cv.survey) -- the data the EM actually fits
#'
#' The data_list is assembled with the pre-clean column names and then handed to
#' the exported Rceattle::clean_data() + Rceattle::switch_check() to normalise
#' names, fill fleet_control defaults, and produce a fit_mod()-ready object.
#' This uses only the released public Rceattle API -- no build_data() /
#' data_requirements(), which are dev-branch only.
#'
#' The OM configuration mirrored here is the canonical one-population case:
#' single species, single sex, one fishery + one survey, age composition only
#' (no length / CAAL), age- and time-invariant weight-at-age, logistic
#' selectivity.
#'
#' @param om_input,om_output,em_input The three lists injected by
#'   load("OM{i}.RData"). `om_output` is accepted for interface symmetry with
#'   prepare_data_fims() and for optional self-tests; the EM data come from
#'   `om_input` (dimensions / biology) and `em_input` (observations).
#' @param proj_years Number of projection years appended after the terminal
#'   year. Rceattle requires `projyr > endyr`; a short horizon is enough for a
#'   hindcast fit (estimateMode = 1). Increase when reference points from an HCR
#'   projection (estimateMode = 0) are wanted.
#' @param sigmaR_floor Lower bound applied to the OM `logR_sd` when seeding
#'   `sigma_rec_prior`. The deterministic OM case (C0) has `logR_sd == 0`, which
#'   is a degenerate recruitment SD; the floor keeps the parameter well-defined.
#'   Recorded on the result as `attr(., "sigmaR_true")` /
#'   `attr(., "sigmaR_floored")` so callers can see whether the floor bit.
#' @param survey_units Which OM survey observation to fit. `"numbers"`
#'   (default) uses `em_input$survey.obs`; `"biomass"` uses
#'   `em_input$surveyB.obs`. These are NOT interchangeable: run_om.R builds
#'   `survey_index` as selectivity-weighted numbers mean-normalised to 1, and
#'   `survey_index_biomass` as the same numbers additionally weighted by
#'   weight-at-age, so the biomass series differs in scale AND in shape --
#'   it reweights toward old heavy ages and diverges whenever age structure
#'   shifts. AMAK/ASAP/BAM/SS fit numbers; run_fims.R currently fits biomass.
#'   Errors if the requested field is absent rather than silently substituting
#'   the other one.
#'
#' @return An Rceattle `data_list` (a plain list) ready for `fit_mod()`.
#' @keywords internal
#' @noRd
om_to_rceattle <- function(om_input, om_output = NULL, em_input,
                           proj_years = 1L, sigmaR_floor = 0.05,
                           survey_units = c("numbers", "biomass")) {

  survey_units <- match.arg(survey_units)

  # ---- Dimensions -----------------------------------------------------------
  years  <- om_input[["year"]]
  nyrs   <- length(years)
  nages  <- om_input[["nages"]]
  ages   <- om_input[["ages"]]
  minage <- min(ages)

  age_cols  <- paste0("Age",  seq_len(nages))
  comp_cols <- paste0("Comp_", seq_len(nages))

  # ---- Observation error (CV -> lognormal SD) -------------------------------
  # cv.L / cv.survey may be scalars or length-nyrs vectors. cv_2_sd() is
  # run_fims.R's helper, reused so observation error matches across EMs.
  catch_log_sd <- cv_2_sd(rep_len(unlist(em_input[["cv.L"]])[1],      nyrs))
  index_log_sd <- cv_2_sd(rep_len(unlist(em_input[["cv.survey"]])[1], nyrs))

  # ---- Survey observation field --------------------------------------------
  # Chosen explicitly rather than detected: OM_ObservationModel.R returns both
  # survey.obs and surveyB.obs, and the two indices differ in units and shape.
  if (survey_units == "biomass") {
    obs <- em_input[["surveyB.obs"]]
    if (is.null(obs) || length(obs) < 1L || length(obs[[1L]]) != nyrs) {
      stop("survey_units = 'biomass' was requested but em_input$surveyB.obs is ",
           "absent or not of length nyr. Regenerate the OM (OM_ObservationModel.R ",
           "does return surveyB.obs) or use survey_units = 'numbers'.",
           call. = FALSE)
    }
    survey_units_code <- 1L   # biomass
  } else {
    obs <- em_input[["survey.obs"]]
    if (is.null(obs) || length(obs) < 1L || length(obs[[1L]]) != nyrs) {
      stop("em_input$survey.obs is absent or not of length nyr.", call. = FALSE)
    }
    survey_units_code <- 2L   # numbers
  }
  survey_obs <- as.numeric(obs[[1L]])

  # ---- Composition sample sizes --------------------------------------------
  # n.L / n.survey are per-year annual sample sizes (constant in these OMs).
  fishery_n <- rep_len(unlist(em_input[["n.L"]])[1],      nyrs)
  survey_n  <- rep_len(unlist(em_input[["n.survey"]])[1], nyrs)

  # ---- Recruitment SD --------------------------------------------------------
  sigmaR_true    <- as.numeric(om_input[["logR_sd"]])
  sigmaR_floored <- max(sigmaR_true, sigmaR_floor)

  simData <- list()

  # ---- Scalars / dimension vectors (nspp == 1) ------------------------------
  simData$nspp                     <- 1L
  simData$styr                     <- min(years)
  simData$endyr                    <- max(years)
  simData$projyr                   <- max(years) + as.integer(proj_years)
  simData$spnames                  <- "OM_species"
  simData$nsex                     <- 1L
  simData$spawn_month              <- 0            # SSB at the start of the year
  simData$nages                    <- nages
  simData$minage                   <- minage
  simData$nlengths                 <- nages        # age-based: no length bins
  simData$pop_wt_index             <- 1L
  simData$ssb_wt_index             <- 1L
  simData$alpha_wt_len             <- 0.0001
  simData$beta_wt_len              <- 3
  simData$pop_age_transition_index <- 1L
  simData$sigma_rec_prior          <- sigmaR_floored
  simData$other_food               <- 1e6
  simData$estDynamics              <- 0

  # ---- Fleet control: fleet 1 = fishery, fleet 2 = survey -------------------
  # Columns and integer switch codes match the released bundled-data schema
  # (e.g. data(GOApollock)$fleet_control), i.e. the post-read_data form the
  # model consumes internally. clean_data() + switch_check() below fill the
  # remaining defaults. Fleet_code MUST equal the row number (data_check()).
  #
  # Switch codes used here (see Rceattle::switch_check / R/0-switches.R):
  #   Fleet_type   Fishery = 1, Survey = 2
  #   Selectivity  Logistic = 1
  #   Comp_loglike Multinomial = 0
  #   Catchability Analytical = 3  (closed-form MLE q; robust, no init needed)
  #
  # The survey uses Analytical catchability rather than a freely estimated q so
  # the fit is not sensitive to a q starting value. FIMS estimates log_q as a
  # free parameter; the resulting q values are directly comparable, but the
  # treatment differs -- a coordination item for the manuscript.
  simData$fleet_control <- data.frame(
    Fleet_name                = c("Fishery", "Survey"),
    Fleet_code                = 1:2,
    Fleet_type                = c(1L, 2L),
    Species                   = 1,
    Month                     = 0,
    Selectivity_index         = 1:2,
    Selectivity               = 1L,                 # Logistic
    N_sel_bins                = NA,
    Sel_curve_pen1            = NA,
    Sel_curve_pen2            = NA,
    Time_varying_sel          = 0,
    Time_varying_sel_sd_prior = 1,
    Bin_first_selected        = 1,
    Sel_norm_bin1             = NA,
    Sel_norm_bin2             = NA,
    Comp_loglike              = 0L,                 # Multinomial
    Comp_weights              = 1,
    CAAL_loglike              = 0,
    Weight1_Numbers2          = c(1L, survey_units_code),  # fishery biomass; survey per units
    Weight_index              = 1,
    Age_transition_index      = 1,
    Q_index                   = c(NA, 1),
    Catchability              = c(NA, 3L),          # survey: Analytical q
    Q_prior                   = c(NA, 1),
    Q_sd_prior                = c(NA, 0.2),
    Time_varying_q            = c(NA, 0),
    Time_varying_q_sd_prior   = c(NA, 1),
    Estimate_index_sd         = c(NA, 0),
    Index_sd_prior            = c(NA, 1),
    Estimate_catch_sd         = c(0, NA),
    Catch_sd_prior            = c(1, NA),
    proj_F_prop               = c(1, NA),
    stringsAsFactors          = FALSE
  )

  # ---- Observations ---------------------------------------------------------
  # Fishery landings (biomass, mt)
  simData$catch_data <- data.frame(
    Fleet_name        = "Fishery",
    Fleet_code        = 1,
    Species           = 1,
    Year              = years,
    Month             = 0,
    Selectivity_block = 1,
    Catch             = as.numeric(em_input[["L.obs"]][[1]]),
    Log_sd            = catch_log_sd,
    stringsAsFactors  = FALSE
  )

  # Survey index
  simData$index_data <- data.frame(
    Fleet_name        = "Survey",
    Fleet_code        = 2,
    Species           = 1,
    Year              = years,
    Month             = 0,
    Selectivity_block = 1,
    Observation       = survey_obs,
    Log_sd            = index_log_sd,
    stringsAsFactors  = FALSE
  )

  # ---- Age composition (proportions; one block per fleet x year) ------------
  # em_input comps are already row-normalised proportions (verified against the
  # OM). Sample_size carries the annual effective N for the multinomial.
  make_comp_block <- function(prop_mat, fleet_name, fleet_code, samp_n) {
    prop_mat <- as.matrix(prop_mat)
    colnames(prop_mat) <- comp_cols
    cbind(
      data.frame(
        Fleet_name   = fleet_name,
        Fleet_code   = fleet_code,
        Species      = 1L,
        Sex          = 0L,
        Age0_Length1 = 0L,      # age composition
        Year         = years,
        Month        = 0,
        Sample_size  = samp_n,
        stringsAsFactors = FALSE
      ),
      as.data.frame(prop_mat)
    )
  }

  simData$comp_data <- rbind(
    make_comp_block(em_input[["L.age.obs"]][["fleet1"]], "Fishery", 1L, fishery_n),
    make_comp_block(em_input[["survey.age.obs"]][[1]],   "Survey",  2L, survey_n)
  )

  # ---- Empty optional blocks (age-based, no length / CAAL / diet) -----------
  caal_cols <- c("Fleet_name", "Fleet_code", "Species", "Sex", "Year",
                 "Length", "Sample_size", paste0("CAAL_", seq_len(nages)))
  simData$caal_data <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = length(caal_cols))), caal_cols)

  simData$emp_sel <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 5 + nages)),
    c("Fleet_name", "Fleet_code", "Species", "Sex", "Year", comp_cols))

  simData$NByageFixed <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 4 + nages)),
    c("Species_name", "Species", "Sex", "Year", age_cols))

  # ---- Age-transition & ageing-error (identity: age-based, no ageing error) --
  age_transition <- as.data.frame(diag(nages))
  colnames(age_transition) <- paste0("Length_", seq_len(nages))
  simData$age_trans_matrix <- cbind(
    data.frame(Age_transition_name = "Base", Age_transition_index = 1,
               Species = 1, Sex = 0, Age = minage:(minage + nages - 1L)),
    age_transition)

  age_error <- as.data.frame(diag(nages))
  colnames(age_error) <- paste0("Obs_age", seq_len(nages))
  simData$age_error <- cbind(
    data.frame(Species = 1, True_age = minage:(minage + nages - 1L)), age_error)

  # ---- Biology --------------------------------------------------------------
  # Weight-at-age: age- and time-invariant (Year = 0 broadcasts to all years).
  waa <- as.data.frame(matrix(as.numeric(om_input[["W.mt"]]), nrow = 1))
  colnames(waa) <- age_cols
  simData$weight <- cbind(
    data.frame(Wt_name = "Base", Wt_index = 1, Species = 1, Sex = 0, Year = 0), waa)

  mat <- as.data.frame(matrix(as.numeric(om_input[["mat.age"]]), nrow = 1))
  colnames(mat) <- age_cols
  simData$maturity <- cbind(data.frame(Species = 1), mat)

  # proportion.female is length-nages (or scalar); recycle to age vector.
  sr <- as.data.frame(matrix(rep_len(as.numeric(om_input[["proportion.female"]]), nages),
                             nrow = 1))
  colnames(sr) <- age_cols
  simData$sex_ratio <- cbind(data.frame(Species = 1), sr)

  m1 <- as.data.frame(matrix(as.numeric(om_input[["M.age"]]), nrow = 1))
  colnames(m1) <- age_cols
  simData$M1_base <- cbind(data.frame(Species = 1, Sex = 0), m1)

  # ---- Bioenergetics / diet placeholders (unused in single-species) ---------
  simData$Ceq <- 1; simData$Cindex <- 1; simData$Pvalue <- 1; simData$fday <- 1
  simData$CA <- 1; simData$CB <- 1; simData$Qc <- 1
  simData$Tco <- 1; simData$Tcm <- 1; simData$Tcl <- 1; simData$CK1 <- 1; simData$CK4 <- 1
  simData$Diet_loglike <- 1; simData$Diet_comp_weights <- 1

  simData$env_data <- data.frame(Year = years, Index1 = 0)

  # Base-R column select rather than a dplyr pipe: keeps dplyr (and its NSE
  # symbols) out of the parallel workers, and avoids the native pipe operator,
  # which does not parse under the R (>= 3.5.0) declared in DESCRIPTION.
  ration_keep <- c("Species", "Sex", "Year",
                   grep("Age", names(simData$weight), value = TRUE,
                        ignore.case = TRUE))
  simData$ration_data <- simData$weight[, ration_keep, drop = FALSE]

  simData$diet_data <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 9)),
    c("Pred", "Prey", "Pred_sex", "Prey_sex", "Pred_age", "Prey_age",
      "Year", "Sample_size", "Stomach_proportion_by_weight"))

  # ---- Normalise + fill defaults via the exported public helpers ------------
  simData <- Rceattle::clean_data(simData)
  simData <- suppressMessages(Rceattle::switch_check(simData))

  attr(simData, "sigmaR_true")    <- sigmaR_true
  attr(simData, "sigmaR_floored") <- sigmaR_floored
  attr(simData, "survey_units")   <- survey_units
  simData
}


#' Seed Rceattle starting parameters at OM truth
#'
#' Initialise the recruitment and rec-dev parameters before fitting.
#'
#' @param data_list An Rceattle data_list from om_to_rceattle().
#' @param om_input The OM truth list (for R0 and logR.resid).
#' @return A parameter list suitable for `fit_mod(inits = )`.
#' @keywords internal
#' @noRd
seed_rceattle_inits <- function(data_list, om_input) {
  p <- suppressWarnings(Rceattle::build_params(data_list))
  p$rec_pars[1, "R0"] <- log(om_input[["R0"]])            # log-scale R0
  resid <- as.numeric(om_input[["logR.resid"]])
  p$rec_dev[1, seq_along(resid)] <- resid
  p
}


#' Beverton-Holt starting values implied by the OM
#'
#' Rceattle parameterizes Beverton-Holt as \eqn{R = \alpha S / (1 + \beta S)},
#' the same form WHAM uses, so alpha and beta follow from the OM's steepness,
#' unfished spawning biomass per recruit, and unfished recruitment:
#'   \deqn{\alpha = 4h / (\phi_0 (1 - h)), \qquad \beta = (\alpha - 1/\phi_0)/R_0.}
#'
#' Rceattle's default starting values carry no information about the stock's
#' scale, and beta in particular has to be of order 1e-3 here for the curve to
#' be near the data.
#'
#' phi0 is `om_input$Phi.0` directly, where `run_wham.R` uses `Phi.0 * 1000`
#' because WHAM counts fish in thousands; the two alphas differ by that factor.
#'
#' @param om_input The OM truth list.
#' @return A list with `alpha` and `beta`, on the natural scale.
#' @keywords internal
#' @noRd
rceattle_bh_pars <- function(om_input) {
  pull <- function(nm) {
    v <- suppressWarnings(as.numeric(om_input[[nm]]))
    if (length(v) < 1L || !is.finite(v[1L])) {
      stop("om_input$", nm, " is missing or not finite; cannot derive ",
           "Beverton-Holt starting values.", call. = FALSE)
    }
    if (length(v) > 1L) {
      warning("om_input$", nm, " has length ", length(v),
              "; using the first element.", call. = FALSE)
    }
    v[1L]
  }
  h    <- pull("h")
  phi0 <- pull("Phi.0")
  R0   <- pull("R0")

  # Below h = 0.2 the curve sits under the replacement line: beta goes negative,
  # recruitment turns negative above S = 1/|beta|, and the implied unfished
  # recruitment is negative. At exactly 0.2 beta is zero and the curve loses its
  # compensation entirely. h = 1 is the asymptote.
  if (h <= 0.2 || h >= 1) {
    stop("Steepness must be in (0.2, 1) for a Beverton-Holt curve; ",
         "om_input$h = ", h, ".", call. = FALSE)
  }
  if (phi0 <= 0 || R0 <= 0) {
    stop("om_input$Phi.0 and om_input$R0 must be positive; got ", phi0,
         " and ", R0, ".", call. = FALSE)
  }

  # The operating model can also be configured with a Ricker curve, which these
  # starting values do not describe.
  srm <- suppressWarnings(as.numeric(om_input[["SRmodel"]])[1])
  if (!is.na(srm) && srm != 1) {
    warning("om_input$SRmodel = ", srm, " is not Beverton-Holt; the Rceattle ",
            "estimation model is fit as Beverton-Holt regardless.",
            call. = FALSE)
  }

  alpha <- 4 * h / (phi0 * (1 - h))
  list(alpha = alpha, beta = (alpha - 1 / phi0) / R0)
}


#' Fit one Rceattle recruitment scenario
#'
#' @param data_list Rceattle data_list.
#' @param inits Initialized parameter list from seed_rceattle_inits().
#' @param scenario One of RCEATTLE_SCENARIOS.
#' @param srr_pars Beverton-Holt alpha / beta from rceattle_bh_pars().
#' @param initMode Initial-age-structure mode. 0 ("FreeParams") estimates the
#'   year-1 age structure as free parameters, which is what WHAM
#'   (`N1_model = 0`, 12 free age-specific N1) and FIMS (`log_init_naa` as fixed
#'   effects) both do.
#' @param time_limit Wall-clock seconds after which a fit is abandoned (returns
#'   NULL, recorded as non-converged). Guards against the pathological
#'   random-effects fit on a deterministic OM case (true sigmaR = 0), where the
#'   Laplace inner problem is ill-conditioned and the optimiser can stall. The
#'   budget covers the estimateMode = 3 map build as well as the fit itself.
#' @return A fitted object of class "Rceattle" (with `$run_time` populated by a
#'   wrapping system.time()), or NULL on error / time-out.
#' @keywords internal
#' @noRd
fit_rceattle_scenario <- function(data_list, inits, scenario, srr_pars,
                                  initMode = 0, time_limit = 300) {
  # Set at function scope, not inside the tryCatch expression below: an on.exit
  # registered from a lazily-evaluated tryCatch argument attaches to tryCatch's
  # frame rather than this one, so the limit could outlive the call.
  setTimeLimit(elapsed = time_limit, transient = TRUE)
  on.exit(setTimeLimit(), add = TRUE)

  random_rec <- scenario %in%
    c("random_effects", "random_effects_sigmaR_constant")

  # Optimiser controls. Tighter and cheaper than the fit_control() defaults
  # (loopnum = 5, getJointPrecision = TRUE).
  ctl <- Rceattle::fit_control(
    getsd = TRUE, getJointPrecision = FALSE,
    loopnum = 1, newtonsteps = 1, phase = FALSE, verbose = 0)

  # Beverton-Holt with alpha fixed at the value implied by the OM's steepness;
  # srr_est_mode = 0 is Rceattle's "fix alpha to the prior mean". WHAM does the
  # same thing explicitly, fixing log_SR_a from the true h and Phi.0 and
  # estimating only beta (run_wham.R). FIMS holds steepness constant too, but at
  # its own package default of 0.75 rather than at the OM's value -- the two
  # coincide for these cases and would not for an OM with a different steepness.
  #
  # Estimating alpha and beta jointly is not an option here: it is singular on
  # this data (reciprocal condition number ~2e-39).
  common <- list(
    data_list  = data_list,
    inits      = inits,
    msmMode    = 0,                                  # single species
    random_rec = random_rec,
    recFun     = Rceattle::build_srr(
      srr_fun        = 2,                            # Beverton-Holt
      srr_est_mode   = 0,                            # fix alpha at the prior mean
      srr_prior      = srr_pars$alpha,
      srr_alpha_init = srr_pars$alpha,
      srr_beta_init  = srr_pars$beta),
    initMode   = initMode
  )

  # Parameter map. fit_mod() builds the map internally when `map = NULL`, which
  # is what the two default scenarios want (random_effects: R_log_sd estimated;
  # fixed_effects: random_rec = FALSE already maps R_log_sd off and treats
  # rec_dev as penalised fixed effects). For the sigmaR-constant scenario we
  # keep rec_dev as random effects but hold R_log_sd fixed at its initial value
  # log(sigma_rec_prior).
  map <- NULL
  if (scenario == "random_effects_sigmaR_constant") {
    # The map build runs under the same time limit as the fit, so it needs the
    # same handler: without it a time-out here escapes fit_rceattle_scenario(),
    # aborts the whole %dopar%, and takes every other replicate with it.
    build <- tryCatch(
      do.call(Rceattle::fit_mod, c(common, list(
        estimateMode = 3,
        fit_control  = Rceattle::fit_control(getsd = FALSE, verbose = 0)))),
      error = function(e) {
        message(sprintf("  [%s] map build error: %s", scenario, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(build)) return(NULL)
    map <- build$map
    map$mapFactor$R_log_sd <- factor(rep(NA, length(map$mapFactor$R_log_sd)))
    if (!is.null(map$mapList)) map$mapList$R_log_sd[] <- NA
  }

  fit <- NULL
  elapsed <- system.time({
    fit <- tryCatch(
      do.call(Rceattle::fit_mod, c(common, list(
        map          = map,
        estimateMode = 1,                        # hindcast (no HCR projection)
        fit_control  = ctl))),
      error = function(e) {
        message(sprintf("  [%s] fit_mod error: %s", scenario, conditionMessage(e)))
        NULL
      }
    )
  })

  if (!is.null(fit)) attr(fit, "elapsed_total") <- elapsed[["elapsed"]]
  fit
}


#' Pull an [spp, sex, age, year] Rceattle array down to an age x year matrix
#'
#' matrix() rather than drop-indexing, so a single-age or single-year model
#' keeps its dimensions.
#'
#' @param x A 4-d Rceattle quantity, e.g. `fit$quantities$N_at_age`.
#' @param yi Year indices to keep (drops the trailing projection column).
#' @param i First-dimension index (species or fleet).
#' @return A `nages x length(yi)` numeric matrix.
#' @keywords internal
#' @noRd
rceattle_age_year <- function(x, yi, i = 1L) {
  nages <- dim(x)[3L]
  matrix(x[i, 1L, , yi], nrow = nages, ncol = length(yi))
}


#' Extract a tidy estimates table from a fitted Rceattle model
#'
#' Comparable in spirit to FIMS::get_estimates(): one row per (quantity, year)
#' with a point estimate and standard error. This is the primary downstream
#' product -- the model-comparison-project scripts read it to build the
#' spawning_biomass / recruitment / abundance / fishing_mortality / catchability
#' panels, so the label set here is a contract with those scripts.
#'
#' Standard errors come from the sdreport for the ADREPORTed quantities (ssb,
#' biomass, R); F, abundance and catchability are not ADREPORTed in the
#' production TMB template, so their uncertainty is NA.
#'
#' Dimension notes: every population array is dimensioned styr:projyr and
#' om_to_rceattle() sets projyr = endyr + 1, so biomass / ssb / R / N_at_age /
#' F_at_age all carry a trailing projection column that `yr_idx` drops.
#' `index_q`, by contrast, is dimensioned [fleet, hindcast year] and carries no
#' projection column.
#'
#' @param fit A fitted "Rceattle" object (single species).
#' @param nyrs Number of hindcast years to report (defaults to all model years).
#' @return A data.frame with columns
#'   `label, year, age, estimate, uncertainty` (uncertainty = SE).
#' @keywords internal
#' @noRd
rceattle_estimates <- function(fit, nyrs = NULL) {
  q <- fit$quantities
  model_yrs <- fit$data_list$styr:fit$data_list$endyr
  if (is.null(nyrs)) nyrs <- length(model_yrs)
  yr_idx <- seq_len(nyrs)
  years  <- model_yrs[yr_idx]

  # sdreport SEs (named vectors in fit$sdrep$value / $sd)
  se_of <- function(name) {
    if (is.null(fit$sdrep)) return(rep(NA_real_, nyrs))
    hits <- which(names(fit$sdrep$value) == name)
    if (!length(hits)) return(rep(NA_real_, nyrs))
    se <- fit$sdrep$sd[hits]
    length(se) <- length(model_yrs)      # guard against proj-year padding
    se[yr_idx]
  }

  mk <- function(label, est, se) {
    data.frame(label = label, year = years, age = NA_integer_,
               estimate = as.numeric(est)[yr_idx],
               uncertainty = as.numeric(se),
               stringsAsFactors = FALSE)
  }

  # Apical F, matching the OM truth this is compared against
  # (apply(om_output$FAA, 1, max)). F_spp is sum_flt exp(log_F), which equals
  # apical F only when selectivity maxes at exactly 1 -- true asymptotically for
  # the logistic form used here, false for a non-parametric selectivity.
  faa <- rceattle_age_year(q$F_at_age, yr_idx)
  naa <- rceattle_age_year(q$N_at_age, yr_idx)

  # Survey catchability. index_q is [fleet, hindcast year] -- no projection
  # column, unlike the population arrays -- and its rows follow fleet_control
  # row order. Analytical (closed-form MLE) q, so this is derived rather than a
  # free parameter; see the fleet_control comment in om_to_rceattle().
  #
  # Fleet_type is the integer switch code (2 = survey) before switch_check() and
  # the readable label ("Survey") after it; a fit carries the cleaned data_list.
  ftype <- fit$data_list$fleet_control$Fleet_type
  is_survey <- if (is.character(ftype)) {
    tolower(ftype) == "survey"
  } else {
    as.integer(ftype) == 2L
  }
  if (!any(is_survey)) stop("No survey fleet found in fleet_control.", call. = FALSE)
  index_q <- as.numeric(q$index_q[which(is_survey)[1L], ])

  rbind(
    mk("SSB",          q$ssb[1, yr_idx],        se_of("ssb")),
    mk("biomass",      q$biomass[1, yr_idx],    se_of("biomass")),
    mk("recruitment",  q$R[1, yr_idx],          se_of("R")),
    mk("F",            apply(faa, 2L, max),     rep(NA_real_, nyrs)),
    mk("abundance",    apply(naa, 2L, sum),     rep(NA_real_, nyrs)),
    mk("catchability", index_q,                 rep(NA_real_, nyrs))
  )
}


#' Count NA standard errors across a sdreport (fixed + random + report)
#'
#' Mirrors the run_fims.R helper of the same name; prefixed to avoid shadowing
#' it (see the file header).
#'
#' @param sdreport A TMB sdreport object, or NULL.
#' @return Integer count of NA standard errors, or NA_integer_ if no sdreport.
#' @keywords internal
#' @noRd
rceattle_count_na_standard_errors <- function(sdreport) {
  if (is.null(sdreport)) return(NA_integer_)
  total_na <- 0L
  for (which_par in c("fixed", "random", "report")) {
    # A fixed-effects model has an empty "random" summary, which warns; that is
    # expected, so silence it.
    s <- tryCatch(suppressWarnings(summary(sdreport, which_par)),
                  error = function(e) NULL)
    if (!is.null(s) && nrow(s) > 0) {
      total_na <- total_na + sum(is.na(s[, "Std. Error"]))
    }
  }
  total_na
}


#' Condition number of the Hessian
#'
#' Same quantity as the run_fims.R helper (kappa of the Hessian), but read from
#' the value Rceattle already computes inside fit_mod() during its own
#' convergence check. Never calls obj$env$spHess(), which can segfault
#' uncatchably for random-effects models. Prefixed to avoid shadowing
#' run_fims.R's three-argument helper (see the file header).
#'
#' @param fit A fitted "Rceattle" object.
#' @return The condition number, or NA_real_ if unavailable.
#' @keywords internal
#' @noRd
rceattle_condition_number <- function(fit) {
  cn <- tryCatch(
    fit$convergence$checks$hessian_conditioning$data$condition_number,
    error = function(e) NULL)
  if (!is.null(cn) && is.finite(cn)) return(as.numeric(cn))
  # Safe fallback: dense Hessian, fixed-effects models only (never spHess).
  if (length(fit$obj[["env"]][["random"]]) == 0L) {
    return(tryCatch(kappa(as.matrix(fit$obj$he(fit$opt$par))),
                    error = function(e) NA_real_))
  }
  NA_real_
}


#' Save the standard result files for one fitted scenario
#'
#' Writes the same set of RDS files run_fims() / run_wham() write, tagged
#' `rceattle` and suffixed by scenario, into `outdir`.
#'
#' @param fit A fitted "Rceattle" object, or NULL on error / time-out.
#' @param scenario One of RCEATTLE_SCENARIOS.
#' @param outdir Destination directory (`casedir/output/<subdir>/s{i}`).
#' @param nyrs Number of hindcast years to report.
#' @param save_full_fit Also write the whole fitted object.
#' @return Invisibly NULL.
#' @keywords internal
#' @noRd
save_rceattle_outputs <- function(fit, scenario, outdir, nyrs = NULL,
                                  save_full_fit = FALSE) {
  tag <- function(stub) file.path(outdir, sprintf("%s_%s.RDS", stub, scenario))

  # A NULL fit still records a non-convergence so downstream summaries see it.
  if (is.null(fit)) {
    saveRDS(NULL,        tag("fit_rceattle"))
    saveRDS(1L,          tag("optimizer_convergence_rceattle"))  # 1 = not converged
    saveRDS(NA_real_,    tag("max_gradient_rceattle"))
    return(invisible(NULL))
  }

  # Estimates table (primary downstream product)
  saveRDS(rceattle_estimates(fit, nyrs), tag("fit_rceattle"))

  # Full fit object, as a fallback for anything the table omits. Off by default:
  # ~200 KB per scenario, or ~300 MB over 500 replicates x 3 scenarios.
  if (isTRUE(save_full_fit)) saveRDS(fit, tag("full_fit_rceattle"))

  # Run time: the same 4-name vector FIMS uses. Rceattle reports one model time
  # on $run_time (a difftime, forced to seconds) and does not separate
  # optimisation from sdreport, so those two fields are NA. `total` is wall-clock
  # elapsed (optimisation + sdreport + overhead), in seconds.
  rt_secs <- if (!is.null(fit$run_time)) as.numeric(fit$run_time, units = "secs") else NA_real_
  total   <- attr(fit, "elapsed_total")
  total   <- if (!is.null(total)) as.numeric(total) else NA_real_
  run_time <- c(
    fit_optimization = NA_real_,
    fit_sdreport     = NA_real_,
    fit_total        = rt_secs,
    total            = total
  )
  saveRDS(run_time, tag("run_time_rceattle"))

  # Max absolute (marginal) gradient. Rceattle stores the final value on
  # fit$opt$max_gradient (via TMBhelper::fit_tmb); fall back to obj$gr() only if
  # that field is absent. Rceattle's opt carries NO nlminb `convergence` field.
  maxgrad <- tryCatch(fit$opt$max_gradient, error = function(e) NULL)
  if (is.null(maxgrad)) {
    maxgrad <- tryCatch(max(abs(fit$obj$gr())), error = function(e) NA_real_)
  }
  maxgrad <- as.numeric(maxgrad)
  saveRDS(maxgrad, tag("max_gradient_rceattle"))

  # Convergence code (0 = converged). Derived to match the convergence checks
  # downstream, which treat a maximum gradient < 0.1 together with a
  # positive-definite Hessian as converged.
  pdhess <- !is.null(fit$sdrep) && isTRUE(fit$sdrep$pdHess)
  conv <- if (!is.na(maxgrad) && maxgrad < 0.1 && pdhess) 0L else 1L
  saveRDS(conv, tag("optimizer_convergence_rceattle"))

  # Hessian-based diagnostics (only when a sdreport is available). NOTE: the
  # na_count_ and condition_number_ stems deliberately carry no model infix --
  # run_fims.R names them the same way, and each EM writes into its own s{i}
  # directory.
  if (!is.null(fit$sdrep)) {
    saveRDS(isTRUE(fit$sdrep$pdHess), tag("hessian_rceattle"))
    saveRDS(rceattle_count_na_standard_errors(fit$sdrep),
            file.path(outdir, sprintf("na_count_%s.RDS", scenario)))
    saveRDS(rceattle_condition_number(fit),
            file.path(outdir, sprintf("condition_number_%s.RDS", scenario)))
  }
  invisible(NULL)
}


#' Run the Rceattle Estimation Model (EM)
#'
#' Reads each operating-model replicate and fits Rceattle over three recruitment
#' scenarios, writing one set of RDS result files per replicate and scenario.
#'
#' All three scenarios fit a Beverton-Holt stock-recruit curve
#' (`build_srr(srr_fun = 2)`, i.e. `R = alpha * S / (1 + beta * S)`) with alpha
#' fixed at the value implied by the operating model's steepness, and with the
#' year-1 age structure estimated as free parameters (`initMode = 0`).
#'
#' Both choices follow the other estimation models. WHAM fixes its Beverton-Holt
#' `log_SR_a` from the operating model's steepness and estimates 12 free
#' age-specific initial numbers-at-age; FIMS estimates `log_init_naa` as fixed
#' effects and holds steepness constant at its package default of 0.75, which
#' matches these operating models but would not match one with a different
#' steepness. Alpha and beta cannot be estimated jointly on these data -- the
#' Hessian is singular -- so fixing one of them is unavoidable for every
#' platform here.
#'
#' Fitting Rceattle with mean recruitment and an equilibrium initial age
#' structure instead would leave it the only model in the comparison without a
#' stock-recruit relationship, which biases terminal spawning-stock biomass high
#' and terminal fishing mortality low.
#'
#' Fits are hindcast-only (`estimateMode = 1`), so no reference points are
#' produced. The scenarios differ only in the treatment of the recruitment
#' deviations: `random_effects` (random effects, sigmaR estimated),
#' `random_effects_sigmaR_constant` (random effects, sigmaR fixed), and
#' `fixed_effects` (penalised fixed effects, sigmaR fixed).
#'
#' On a deterministic OM case (`logR_sd == 0`) the two random-effects scenarios
#' are ill-posed -- the Laplace inner problem for a zero-variance random effect
#' is singular -- and are expected to hit `time_limit` and be recorded as
#' non-converged. Use a stochastic case (C1 / C2) for real comparisons.
#'
#' @param maindir Main working directory. Accepted for signature parity with the
#'   other `run_*()` functions; not used directly.
#' @param subdir Estimation-model output subfolder under `casedir/output`.
#' @param om_sim_num Number of iterations from the operating model
#'   (`OM1.RData` .. `OM{n}.RData`).
#' @param casedir Case working directory, containing `output/OM/OM{i}.RData`.
#' @param em_bias_cor Use bias correction in the estimation model or not?
#'   Accepted for signature parity; Rceattle handles recruitment-deviation bias
#'   correction through its own `fit_control()`.
#' @param survey_units Which OM survey observation to fit: `"numbers"`
#'   (`em_input$survey.obs`, mean-normalised) or `"biomass"`
#'   (`em_input$surveyB.obs`, mt). Errors if the requested field is absent.
#' @param save_full_fit Also write the full fitted object per scenario
#'   (`full_fit_rceattle_<scenario>.RDS`). Off by default; roughly 200 KB per
#'   scenario per replicate.
#' @param time_limit Wall-clock seconds after which one scenario fit is
#'   abandoned and recorded as non-converged.
#'
#' @return Invisibly NULL. Results are written under
#'   `casedir/output/<subdir>/s{i}/`.
#' @export
run_rceattle <- function(maindir = maindir,
                         subdir = "Rceattle",
                         om_sim_num = NULL,
                         casedir = casedir,
                         em_bias_cor = em_bias_cor,
                         survey_units = c("numbers", "biomass"),
                         save_full_fit = FALSE,
                         time_limit = 300) {

  if (!("Rceattle" %in% installed.packages()[, "Package"])) stop("Please install Rceattle!")

  # Force these to plain scalars before the cluster starts, so only values (not
  # unevaluated promises) are exported to the workers.
  survey_units  <- match.arg(survey_units)
  save_full_fit <- isTRUE(save_full_fit)
  time_limit    <- as.numeric(time_limit)

  # Clean and (re)create per-replicate output folders.
  unlink(list.files(file.path(casedir, "output", subdir), full.names = TRUE),
         recursive = TRUE)
  for (x in seq_len(om_sim_num)) {
    dir.create(file.path(casedir, "output", subdir, paste0("s", x)),
               recursive = TRUE, showWarnings = FALSE)
  }

  closeAllConnections()
  cores <- if (parallel::detectCores() == 1) 1 else parallel::detectCores() - 2
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressMessages(library(Rceattle))
    TMB::openmp(n = 1)
  })

  # Ship the helpers to the workers with clusterExport() rather than foreach's
  # .export, which skips symbols it resolves inside a package namespace on the
  # assumption that .packages supplies them -- ASSAMC is loaded with
  # devtools::load_all() and is not installed on the workers. cv_2_sd is
  # run_fims.R's, reused so observation error is specified the same way across
  # estimation models.
  parallel::clusterExport(
    cl,
    c("RCEATTLE_SCENARIOS",
      "om_to_rceattle", "cv_2_sd", "seed_rceattle_inits", "rceattle_bh_pars",
      "fit_rceattle_scenario", "rceattle_age_year",
      "rceattle_estimates", "rceattle_count_na_standard_errors",
      "rceattle_condition_number", "save_rceattle_outputs"),
    envir = environment()
  )

  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(
    om_sim = seq_len(om_sim_num),
    .packages = c("Rceattle")
    # casedir / subdir / survey_units / save_full_fit / time_limit are named
    # directly in the loop body, so foreach exports them automatically; listing
    # them again in .export only produces an "already exporting" warning.
  ) %dopar% {
    # One replicate must not take the other 499 with it. fit_rceattle_scenario()
    # already returns NULL on a failed fit; this covers the data translation and
    # the writing, which can stop() on a malformed OM.
    tryCatch({
      load(file = file.path(casedir, "output", "OM", paste0("OM", om_sim, ".RData")))
      outdir <- file.path(casedir, "output", subdir, paste0("s", om_sim))

      data_list <- om_to_rceattle(om_input, om_output, em_input,
                                  survey_units = survey_units)
      inits     <- seed_rceattle_inits(data_list, om_input)
      srr_pars  <- rceattle_bh_pars(om_input)
      nyrs      <- length(om_input[["year"]])

      for (scenario in RCEATTLE_SCENARIOS) {
        fit <- fit_rceattle_scenario(data_list, inits, scenario, srr_pars,
                                     time_limit = time_limit)
        save_rceattle_outputs(fit, scenario, outdir, nyrs = nyrs,
                              save_full_fit = save_full_fit)
      }
    }, error = function(e) {
      message(sprintf("  [OM %d] skipped: %s", om_sim, conditionMessage(e)))
    })
    NULL
  }
  invisible(NULL)
}
