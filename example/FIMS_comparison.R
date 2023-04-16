## Install required packages
required_pkg <- c(
  "remotes", "devtools", "here",
  "rstudioapi", "gdata", "PBSadmb",
  "stringr", "matrixcalc", "r4ss",
  "ASAPplots",  "readxl", "scales",
  "corrplot",  "glue", "parallel",
  "doParallel"
)
pkg_to_install <- required_pkg[!(required_pkg %in%
                                   installed.packages()[, "Package"])]
if (length(pkg_to_install)) install.packages(pkg_to_install)

invisible(lapply(required_pkg, library, character.only = TRUE))

devtools::load_all()
# remotes::install_github(repo = "Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
library(ASSAMC)

devtools::install_github(
  "NOAA-FIMS/FIMS",
  ref = "e4af7896ee07e0c118e71d651689cc5024a68f33")
library(FIMS)

## Set-up OM (sigmaR = 0.4)
maindir <- file.path(here::here(), "example")
model_input <- save_initial_input()
C2 <- save_initial_input(
  base_case = TRUE,
  input_list = model_input,
  maindir = maindir,
  om_sim_num = 160,
  keep_sim_num = 100,
  figure_number = 10,
  seed_num = 9924,
  case_name = "C2"
)

ASSAMC::run_om(input_list = C2)

ASSAMC::run_em(em_names = c("AMAK", "ASAP", "BAM", "SS", "FIMS"),
               input_list = C2,
               em_input_filenames = data.frame(
                 AMAK = "C0",
                 ASAP = "C0",
                 BAM = "C0",
                 SS = "C1"
               ))

ASSAMC::generate_plot(
  em_names = c("AMAK", "ASAP", "BAM", "SS", "FIMS"),
  plot_ncol=2, plot_nrow=3,
  plot_color = c("orange", "green", "red", "deepskyblue3", "gray"),
  input_list = C2)

