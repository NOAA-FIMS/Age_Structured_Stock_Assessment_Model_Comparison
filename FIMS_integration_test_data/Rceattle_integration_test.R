# Rceattle (https://github.com/grantdadams/Rceattle) integration test
#
# Local development smoke test: fits the Rceattle EM against ONE bundled
# operating-model replicate and self-tests recovery against OM truth. It reuses
# the OM that FIMS_integration_test.R generates, so run that first if
# FIMS_C1/output/OM/OM1.RData is missing.
#
# Not run by R CMD check -- this directory is neither R/ nor tests/.

# Install required packages

required_pkg <- c("devtools", "here")
pkg_to_install <- required_pkg[!(required_pkg %in%
  installed.packages()[, "Package"])]
if (length(pkg_to_install)) install.packages(pkg_to_install)

# setwd() at project root if not there, e.g.,
# setwd("C:/Users/Age_Structured_Stock_Assessment_Model_Comparison/")

devtools::load_all()

if (!("Rceattle" %in% installed.packages()[, "Package"])) {
  stop("Please install Rceattle: remotes::install_github('grantdadams/Rceattle')")
}

maindir <- file.path(here::here(), "FIMS_integration_test_data")

# Case C1 (logR_sd = 0.4). The deterministic cases (FIMS_C0, C0noPhiF) make the
# random-effects scenarios ill-posed, so they are not a useful smoke test.
case    <- "FIMS_C1"
casedir <- file.path(maindir, case)
outdir  <- file.path(casedir, "output", "Rceattle", "s1")

if (!file.exists(file.path(casedir, "output", "OM", "OM1.RData"))) {
  stop("Missing ", file.path(casedir, "output", "OM", "OM1.RData"),
       ". Run FIMS_integration_test.R first to generate the operating model.")
}

# Run the EM ------------------------------------------------------

# Defaults everywhere, so the default code path is what gets exercised.
runtime <- system.time(
  run_rceattle(
    maindir     = maindir,
    om_sim_num  = 1,
    casedir     = casedir,
    em_bias_cor = FALSE
  )
)
cat(sprintf("\nrun_rceattle() wall clock: %.1f s\n\n", runtime[["elapsed"]]))

# Check 1: file manifest ------------------------------------------

# A fit that errored or hit the time limit writes only the three files that
# record the non-convergence; the sdreport diagnostics do not exist for it.
always_stems <- c("fit_rceattle", "optimizer_convergence_rceattle",
                  "max_gradient_rceattle")
converged_stems <- c("run_time_rceattle", "hessian_rceattle")

for (scn in RCEATTLE_SCENARIOS) {
  for (stem in always_stems) {
    f <- file.path(outdir, sprintf("%s_%s.RDS", stem, scn))
    if (!file.exists(f)) stop("Missing expected output: ", f)
  }
  conv <- readRDS(file.path(outdir,
                            sprintf("optimizer_convergence_rceattle_%s.RDS", scn)))
  if (identical(as.integer(conv), 0L)) {
    for (stem in converged_stems) {
      f <- file.path(outdir, sprintf("%s_%s.RDS", stem, scn))
      if (!file.exists(f)) stop("Missing expected output: ", f)
    }
    for (stem in c("na_count", "condition_number")) {
      f <- file.path(outdir, sprintf("%s_%s.RDS", stem, scn))
      if (!file.exists(f)) stop("Missing expected output: ", f)
    }
  } else {
    message("  note: ", scn, " did not converge (code ", conv,
            "); sdreport diagnostics are absent by design.")
  }
  # save_full_fit defaults to FALSE, so the full object must NOT be written.
  f <- file.path(outdir, sprintf("full_fit_rceattle_%s.RDS", scn))
  if (file.exists(f)) stop("save_full_fit = FALSE but found: ", f)
}
cat("Check 1 (file manifest): OK -",
    length(list.files(outdir)), "files in", outdir, "\n")

# Check 2: estimates table schema ---------------------------------

truth <- new.env()
load(file.path(casedir, "output", "OM", "OM1.RData"), envir = truth)
nyr <- truth$om_input$nyr

est <- readRDS(file.path(outdir, "fit_rceattle_random_effects.RDS"))
stopifnot(
  is.data.frame(est),
  identical(names(est), c("label", "year", "age", "estimate", "uncertainty")),
  setequal(unique(est$label),
           c("SSB", "biomass", "recruitment", "F", "abundance", "catchability")),
  all(table(est$label) == nyr),
  !anyNA(est$estimate)
)
cat("Check 2 (estimates schema): OK -", nrow(est), "rows,",
    length(unique(est$label)), "labels x", nyr, "years\n")

# Check 3: recovery of OM truth -----------------------------------

# The EM is compared against the same OM quantities the downstream comparison
# scripts use: apical F, age-1 numbers for recruitment, total numbers for
# abundance, and the numbers-based survey catchability.
om <- list(
  SSB          = truth$om_output$SSB,
  biomass      = truth$om_output$biomass.mt,
  recruitment  = truth$om_output$N.age[, 1],
  F            = apply(truth$om_output$FAA, 1, max),
  abundance    = rowSums(truth$om_output$N.age),
  catchability = rep(truth$om_output$survey_q[["survey1"]], nyr)
)

get_series <- function(estimates, label) {
  x <- estimates[estimates$label == label, ]
  x$estimate[order(x$year)]
}

# Catchability is time-invariant in both the OM and the EM, so its correlation
# is undefined (zero variance); only the ratio is informative there.
safe_cor <- function(x, y) {
  if (stats::sd(x) == 0 || stats::sd(y) == 0) NA_real_ else stats::cor(x, y)
}

cat("\nRecovery vs OM truth:\n")
cat(sprintf("%-32s %10s %10s %10s\n", "scenario", "quantity", "cor", "mean_ratio"))
for (scn in RCEATTLE_SCENARIOS) {
  e <- readRDS(file.path(outdir, sprintf("fit_rceattle_%s.RDS", scn)))
  conv <- readRDS(file.path(outdir,
                            sprintf("optimizer_convergence_rceattle_%s.RDS", scn)))
  cat(sprintf("  %s (convergence code %s)\n", scn, conv))
  for (lab in names(om)) {
    v <- get_series(e, lab)
    cat(sprintf("%-32s %10s %10.4f %10.4f\n", "", lab,
                safe_cor(v, om[[lab]]), mean(v / om[[lab]])))
  }
}

# Assert only on random_effects; fixed_effects legitimately differs and is
# printed above for eyeball comparison. Tolerances are deliberately loose so the
# test is not flaky across Rceattle patch releases -- reference values on a
# clean run are cor 0.9999 / 0.9999 / 0.985 / 0.9986 and mean ratio ~1.03.
e <- readRDS(file.path(outdir, "fit_rceattle_random_effects.RDS"))
stopifnot(
  stats::cor(get_series(e, "SSB"),          om$SSB)          > 0.99,
  stats::cor(get_series(e, "biomass"),      om$biomass)      > 0.99,
  stats::cor(get_series(e, "recruitment"),  om$recruitment)  > 0.95,
  stats::cor(get_series(e, "F"),            om$F)            > 0.98,
  stats::cor(get_series(e, "abundance"),    om$abundance)    > 0.99
)

# Correlation is scale-invariant, so the checks above would pass a kg/mt or
# numbers/thousands regression untouched -- exactly the failure mode this
# integration is exposed to, since the downstream reader divides recruitment and
# abundance by 1000. Every series therefore also gets a magnitude check.
for (lab in names(om)) {
  ratio <- mean(get_series(e, lab) / om[[lab]])
  if (!isTRUE(abs(ratio - 1) < 0.20)) {
    stop(sprintf("%s is off OM truth by a factor of %.4g -- a units regression?",
                 lab, ratio))
  }
}

# Terminal-year and median bias, which is where a structural misspecification
# (initial age structure, or the stock-recruit form) shows up first.
ssb <- get_series(e, "SSB")
re <- function(x, truth) 100 * (x - truth) / truth
cat(sprintf("\n  SSB relative error: year 1 %+.2f%%  median %+.2f%%  year %d %+.2f%%\n",
            re(ssb[1], om$SSB[1]), median(re(ssb, om$SSB)), nyr,
            re(ssb[nyr], om$SSB[nyr])))
stopifnot(
  abs(median(re(ssb, om$SSB))) < 10,
  abs(re(ssb[nyr], om$SSB[nyr])) < 25
)
cat("\nCheck 3 (OM recovery): OK\n")

# Check 4: convergence diagnostics --------------------------------

for (scn in RCEATTLE_SCENARIOS) {
  grad <- readRDS(file.path(outdir, sprintf("max_gradient_rceattle_%s.RDS", scn)))
  pdh  <- readRDS(file.path(outdir, sprintf("hessian_rceattle_%s.RDS", scn)))
  cond <- readRDS(file.path(outdir, sprintf("condition_number_%s.RDS", scn)))
  conv <- readRDS(file.path(outdir,
                            sprintf("optimizer_convergence_rceattle_%s.RDS", scn)))
  cat(sprintf("%-32s conv=%s  max|grad|=%.3g  pdHess=%s  kappa=%.4g\n",
              scn, conv, grad, pdh, cond))
  stopifnot(conv == 0L, grad < 1e-3, isTRUE(pdh), is.finite(cond))
}
cat("\nCheck 4 (convergence): OK\n")

# Check 5: the survey_units guard ---------------------------------

# Which branch is testable depends on the OM. The bundled fixtures predate the
# current OM_ObservationModel.R and carry only survey.obs, so `biomass` must
# error; a regenerated OM does carry surveyB.obs, and then the biomass branch
# must build and be on a visibly different scale from the numbers branch.
has_biomass <- {
  o <- truth$em_input[["surveyB.obs"]]
  !is.null(o) && length(o) >= 1L && length(o[[1L]]) == nyr
}

if (has_biomass) {
  dl_b <- om_to_rceattle(truth$om_input, truth$om_output, truth$em_input,
                         survey_units = "biomass")
  dl_n <- om_to_rceattle(truth$om_input, truth$om_output, truth$em_input,
                         survey_units = "numbers")
  stopifnot(identical(attr(dl_b, "survey_units"), "biomass"),
            identical(attr(dl_n, "survey_units"), "numbers"))
  # The numbers index is mean-normalised to 1; the biomass index is in mt. If
  # these ever agree, the two branches have collapsed into one.
  stopifnot(abs(mean(dl_n$index_data$Observation) - 1) < 0.2,
            abs(mean(dl_b$index_data$Observation) - 1) > 0.2)
  cat("Check 5 (survey_units): OK - both branches build and differ in scale\n")
} else {
  msg <- tryCatch(
    om_to_rceattle(truth$om_input, truth$om_output, truth$em_input,
                   survey_units = "biomass"),
    error = conditionMessage
  )
  stopifnot(is.character(msg), grepl("surveyB.obs", msg))
  cat("Check 5 (survey_units): OK - this OM has no surveyB.obs and the",
      "biomass branch errors rather than silently substituting\n")
}

# Check 6: the estimation model is configured as intended ---------

# The recovery checks above are satisfied by several configurations, including
# the mean-recruitment one this replaced, so assert the configuration directly:
# a Beverton-Holt curve with alpha held at the value implied by the OM's
# steepness, and the initial age structure estimated as free parameters.
srr <- rceattle_bh_pars(truth$om_input)
fit <- fit_rceattle_scenario(
  om_to_rceattle(truth$om_input, truth$om_output, truth$em_input),
  seed_rceattle_inits(om_to_rceattle(truth$om_input, truth$om_output,
                                     truth$em_input), truth$om_input),
  "random_effects", srr)

# alpha fixed (mapped out), beta estimated
map_rec <- fit$map$mapList$rec_pars[1, ]
stopifnot(is.na(map_rec[2]), !is.na(map_rec[3]))
# and fixed AT the OM-implied value, not at a package default
stopifnot(isTRUE(all.equal(unname(exp(fit$obj$env$parList()$rec_pars[1, 2])),
                           srr$alpha)))
# initial age structure estimated as fixed effects, not integrated
stopifnot(!("init_dev" %in% unique(names(fit$obj$env$par)[fit$obj$env$random])),
          any(!is.na(fit$map$mapList$init_dev)))
cat(sprintf("Check 6 (EM configuration): OK - alpha fixed at %.1f (h = %.2f), ",
            srr$alpha, truth$om_input$h),
    "beta estimated, init_dev free fixed effects\n", sep = "")

cat("\nAll checks passed. Outputs are in", outdir, "(gitignored).\n")

# Optional figure -------------------------------------------------

make_plot <- FALSE
if (make_plot) {
  pdf(file.path(tempdir(), paste0("rceattle_vs_om_", case, ".pdf")),
      width = 11, height = 7)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(2, 3))
  cols <- c(random_effects = "#1b9e77",
            random_effects_sigmaR_constant = "#d95f02",
            fixed_effects = "#7570b3")
  labs <- c(SSB = "Spawning-stock biomass (mt)",
            biomass = "Total biomass (mt)",
            recruitment = "Recruitment (age-1 numbers)",
            F = "Fully-selected F",
            abundance = "Total abundance (numbers)",
            catchability = "Survey catchability")
  for (lab in names(labs)) {
    plot(seq_len(nyr), om[[lab]], type = "l", lwd = 3, col = "black",
         xlab = "Year", ylab = labs[[lab]], main = labs[[lab]])
    for (scn in RCEATTLE_SCENARIOS) {
      e <- readRDS(file.path(outdir, sprintf("fit_rceattle_%s.RDS", scn)))
      lines(seq_len(nyr), get_series(e, lab), lwd = 2, lty = 2, col = cols[[scn]])
    }
    if (lab == "SSB") {
      legend("bottomleft", bty = "n", lwd = c(3, 2, 2, 2), lty = c(1, 2, 2, 2),
             col = c("black", cols),
             legend = c("OM truth", RCEATTLE_SCENARIOS))
    }
  }
  cat("Figure written to", tempdir(), "\n")
}
