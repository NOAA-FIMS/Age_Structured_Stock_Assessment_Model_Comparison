# devtools::load_all(file.path("C:", "Users", "bai.li", "Desktop", "FIMS"))
# library(FIMS)

## Install required packages
required_pkg <- c("remotes", "devtools", "here")
pkg_to_install <- required_pkg[!(required_pkg %in%
                                   installed.packages()[, "Package"])]
if (length(pkg_to_install)) install.packages(pkg_to_install)

remotes::install_github(repo = "Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
library(ASSAMC)

list_of_packages <- c("rstudioapi", "gdata", "PBSadmb", "stringr", "matrixcalc", "r4ss", "ASAPplots",  "readxl", "scales", "corrplot",  "glue", "parallel", "doParallel")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
invisible(lapply(list_of_packages, library, character.only = TRUE))

## Set-up OM (sigmaR = 0.4)
maindir <- file.path(here::here(), "example")
model_input <- save_initial_input()
C1 <- save_initial_input(
  base_case = TRUE,
  input_list = model_input,
  maindir = maindir,
  om_sim_num = 160,
  keep_sim_num = 100,
  figure_number = 10,
  seed_num = 9924,
  case_name = "C1"
)

ASSAMC::run_om(input_list = C1)

ASSAMC::run_em(em_names = c("AMAK", "ASAP", "BAM", "SS"),
               input_list = C1,
               em_input_filenames = data.frame(
                 AMAK = "C0",
                 ASAP = "C0",
                 BAM = "C0",
                 SS = "C1"
               ))

