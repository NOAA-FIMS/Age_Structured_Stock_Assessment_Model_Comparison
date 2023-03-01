# Operating model for FIMS (https://github.com/NOAA-FIMS/FIMS) integration test

# Install required packages

required_pkg <- c("remotes", "devtools", "here")
pkg_to_install <- required_pkg[!(required_pkg %in%
  installed.packages()[, "Package"])]
if (length(pkg_to_install)) install.packages(pkg_to_install)

# User will need to pick a "Set up" option below depending on their workflow


# Set up OPTION 1: If user wants to use the ASSAMC package --------

# Fork the repo and then install the package, for example:

remotes::install_github(repo = "JaneSullivan-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")

# Set up OPTION 2: If user is developing within the ASSAMC --------

# Clone the repo from https://github.com/Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison

remotes::install_github(repo = "Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
library(ASSAMC)

# setwd() at project root if not there, e.g.,
# setwd("C:/Users/Age_Structured_Stock_Assessment_Model_Comparison/")

devtools::load_all()

maindir <- file.path(here::here(), "FIMS_integration_test_data")

# Default simulation settings -------------------------------------

# Details of default simulation settings can be found here:
# https://github.com/Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison/blob/master/R/save_initial_input.R

model_input <- save_initial_input()

# Case 0 ----------------------------------------------------------

# Operating model for FIMS test case 0 where logR_sd = 0. All other OM
# settings are the same as case 0 in Li et al. 2021
# https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/fish-bull/11923li.pdf

FIMS_C0 <- save_initial_input(
  base_case = TRUE,
  input_list = model_input,
  maindir = maindir,
  om_sim_num = 1,
  keep_sim_num = 1,
  figure_number = 1,
  seed_num = 9924,
  logR_sd = 0,
  case_name = "FIMS_C0"
)

# Run OM
run_om(input_list = FIMS_C0)


# Case 0_noPhiF ---------------------------------------------------
FIMS_C0noPhiF <- save_initial_input(
  base_case = FALSE,
  input_list = FIMS_C0,
  initial_equilibrium_F = FALSE,
  case_name = "C0noPhiF"
)

run_om(input_list = FIMS_C0noPhiF)

# Case 1 ----------------------------------------------------------

# Modify FIMS_C0 and change logR_sd from 0 to 0.4

FIMS_C1 <- save_initial_input(
  base_case = FALSE,
  input_list = FIMS_C0,
  logR_sd = 0.4,
  case_name = "FIMS_C1"
)

run_om(input_list = FIMS_C1)


# Case 2 ----------------------------------------------------------

# Modify FIMS_C1 and ensure the sum of deviations equals to zero

FIMS_C2 <- save_initial_input(
  base_case = FALSE,
  input_list = FIMS_C1,
  f_dev_sum2zero = TRUE,
  r_dev_sum2zero = TRUE,
  case_name = "FIMS_C2"
)

run_om(input_list = FIMS_C2)

# Aggregate OM outputs ------------------------------------------------------------------------

om_list <- vector(mode = "list", length = 1)
load(file.path(maindir, "FIMS_C0", "output", "OM", paste("OM", 1, ".RData", sep = "")))

om_biomass <- om_abundance <-
  om_ssb <- om_recruit <- om_Ftot <- om_Fmul <-
  om_landing <- om_survey <-
  om_fratio <- om_ssbratio <-
  om_landing_err <- om_survey_err <-
  matrix(NA, nrow = om_input$nyr, ncol = FIMS_C0$om_sim_num)

om_msy <- om_fmsy <- om_ssbmsy <-
  om_geomR0 <- om_arimR0 <-
  om_geomS0 <- om_arimS0 <-
  om_geomDf <- om_arimDf <-
  matrix(NA, nrow = 1, ncol = FIMS_C0$om_sim_num)

om_agecomp <- list()

for (om_sim in 1:FIMS_C0$om_sim_num) {
  load(file.path(maindir, "FIMS_C0", "output", "OM", paste("OM", om_sim, ".RData", sep = "")))

  om_biomass[, om_sim] <- om_output$biomass.mt
  om_abundance[, om_sim] <- om_output$abundance / 1000
  om_ssb[, om_sim] <- om_output$SSB
  om_recruit[, om_sim] <- om_output$N.age[, 1] / 1000
  om_Ftot[, om_sim] <- apply(om_output$FAA, 1, max)
  om_landing[, om_sim] <- om_output$L.mt$fleet1
  om_survey[, om_sim] <- om_output$survey_index$survey1
  om_msy[, om_sim] <- om_output$msy$msy
  om_fmsy[, om_sim] <- unique(round(om_output$msy$Fmsy, digits = 3))
  om_ssbmsy[, om_sim] <- unique(om_output$msy$SSBmsy)
  om_fratio[, om_sim] <- om_Ftot[, om_sim] / om_fmsy[om_sim]
  om_ssbratio[, om_sim] <- om_ssb[, om_sim] / om_ssbmsy[om_sim]
  om_agecomp[[om_sim]] <- apply(om_output$N.age / 1000, 1, function(x) x / sum(x))
  om_geomR0[, om_sim] <- om_input$median_R0 / 1000
  om_arimR0[, om_sim] <- om_input$mean_R0 / 1000
  om_geomS0[, om_sim] <- om_input$median_R0 * om_input$Phi.0
  om_arimS0[, om_sim] <- om_input$mean_R0 * om_input$Phi.0
  om_geomDf[, om_sim] <- om_ssb[nrow(om_ssb), om_sim] / om_geomS0[, om_sim]
  om_arimDf[, om_sim] <- om_ssb[nrow(om_ssb), om_sim] / om_arimS0[, om_sim]
}

om_list <- list(
  om_biomass, om_abundance,
  om_ssb, om_recruit, om_Ftot,
  om_landing, om_survey,
  om_msy, om_fmsy, om_ssbmsy,
  om_fratio, om_ssbratio,
  om_geomR0, om_arimR0,
  om_geomS0, om_arimS0,
  om_geomDf, om_arimDf,
  om_agecomp
)

names(om_list) <- c(
  "biomass", "abundance",
  "ssb", "recruit", "Ftot",
  "landing", "survey",
  "msy", "fmsy", "ssbmsy",
  "fratio", "ssbratio",
  "geomR0", "arimR0",
  "geomS0", "arimS0",
  "geomDf", "arimDf",
  "agecomp"
)


# Plot operating model outputs ---------------------------------------------------------------------------------

var <- c(
  "biomass", "abundance",
  "ssb", "recruit", "Ftot",
  "landing", "survey"
)
ylab <- c(
  "Biomass", "Abundance",
  "SSB", "R", "F",
  "Landings", "Survey Index"
)

for (i in 1:length(var)) {
  ylim <- range(om_list[[var[i]]])

  plot(0,
    type = "n",
    xlab = "Year",
    ylab = ylab[i],
    xlim = range(om_input$year),
    ylim = ylim
  )
  lines(om_input$year, om_list[[var[i]]],
    type = "o"
  )
}


# Plot yield over F ---------------------------------------------------------------------------------
ylim <- range(om_output$msy$L_eq)
xlim <- c(0, 2)

plot(om_output$msy$f_seq,
  om_output$msy$L_eq,
  ylim = ylim, xlim = xlim,
  xlab = "F", ylab = "Yield",
  type = "l", lty = 1, col = 1
)
