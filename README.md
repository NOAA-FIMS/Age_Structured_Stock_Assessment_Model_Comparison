# Age-Structured Stock Assessment Model Comparison Project

We used a simulation-estimation approach to evaluate the reliability of the four primary age structured assessment models currently used by NOAA Fisheries: the Age Structured Assessment Program (ASAP), the Assessment Model for Alaska (AMAK), the Beaufort Assessment Model (BAM), and Stock Synthesis (SS).

## Table of Contents

- [About the project](#About-the-project)
- [Getting started](#Getting-started)
- [Usage](#Usage)
- [Disclaimer](#Disclaimer)

## About the project

The National Oceanic and Atmospheric Administration National Marine Fisheries Service (NOAA Fisheries) conducts annual stock assessments and provides the best scientific information available for fisheries management to the U.S. regional fishery management councils. Different regions utilize different assessment software packages, although the models share similar mathematical and statistical attributes. However, comparison studies identifying similarities and differences among different packages used in the U.S. remain scarce. We evaluated the reliability of the four primary age structured assessment models currently used by NOAA Fisheries: the Age Structured Assessment Program (ASAP), the Assessment Model for Alaska (AMAK), the Beaufort Assessment Model (BAM), and Stock Synthesis (SS). We used a simulation-estimation approach to examine any bias of parameter estimation under various model structures. A paper describing the project has been published:[https://doi.org/10.7755/FB.119.2-3.5](https://doi.org/10.7755/FB.119.2-3.5).

The project is built based on [R](https://www.r-project.org/). 

## Getting started

>**1. Get the age-structured stock assessment packages:**

   - [AMAK](https://github.com/NMFS-toolbox/AMAK.git)
   - [ASAP](https://nmfs-fish-tools.github.io/ASAP/)
   - [BAM](https://repository.library.noaa.gov/view/noaa/4847)
   - [SS](https://vlab.ncep.noaa.gov/web/stock-synthesis/home)

>**2. Install ASSAMC to set up the operating model**

```r
remotes::install_github("Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison")
```

## Usage
```r
# Set up project directory ---------------------------------------------------------------
project_dir <- "XXX"

## Download and keep Age_Structured_Stock_Assessment_Model_Comparison/example/em_input/ folder in the project directory

# Set up OM ---------------------------------------------------------------

# Need to have the em_input folder in the working directory run other estimation models
maindir <- file.path(project_dir)

om_sim_num <- 120 # total number of iterations per case
keep_sim_num <- 100 # number of kept iterations per case
figure_number <- 10 # number of individual iteration to plot

seed_num <- 9924

## Life-history parameters
year <- 1:30
ages <- 1:12 # Age structure of the popn

initial_equilibrium_F <- TRUE
median_R0 <- 1000000 # Average annual unfished recruitment (scales the popn)
median_h <- 0.75 # Steepness of the Beverton-Holt spawner-recruit relationship.
mean_R0 <- NULL
mean_h <- NULL
SRmodel <- 1 # 1=Beverton-Holt; 2=Ricker
M <- 0.2 # Age-invariant natural mortality

Linf <- 800 # Asymptotic average length
K <- 0.18 # Growth coefficient
a0 <- -1.36 # Theoretical age at size 0
a.lw <- 0.000000025 # Length-weight coefficient
b.lw <- 3.0 # Length-weight exponent
A50.mat <- 2.25 # Age at 50% maturity
slope.mat <- 3 # Slope of maturity ogive
pattern.mat <- 1 # Simple logistic maturity
female.proportion <- 0.5 # Sex ratio

## Fleet settings
fleet_num <- 1

# CV of landings for OM
cv.L <- list()
cv.L$fleet1 <- 0.005

# Input CV of landings for EMs
input.cv.L <- list()
input.cv.L$fleet1 <- 0.01

# Annual sample size (nfish) of age comp samples
n.L <- list()
n.L$fleet1 <- 200

# Define fleet selectivity
sel_fleet <- list()

sel_fleet$fleet1$pattern <- 1
sel_fleet$fleet1$A50.sel1 <- 2
sel_fleet$fleet1$slope.sel1 <- 1

## Survey settings
survey_num <- 1

# CV of surveys for OM
cv.survey <- list()
cv.survey$survey1 <- 0.1

# Input CV of surveys for EMs
input.cv.survey <- list()
input.cv.survey$survey1 <- 0.2

# Annual sample size (nfish) of age comp samples
n.survey <- list()
n.survey$survey1 <- 200

# Define survey selectivity
sel_survey <- list()

sel_survey$survey1$pattern <- 1
sel_survey$survey1$A50.sel1 <- 1.5
sel_survey$survey1$slope.sel1 <- 2

## Other settings
logf_sd <- 0.2
f_dev_change <- FALSE
f_pattern <- 1
start_val <- 0.01
middle_val <- NULL
end_val <- 0.39
f_val <- NULL
start_year <- 1
middle_year <- NULL

logR_sd <- 0.4
r_dev_change <- TRUE

om_bias_cor <- FALSE
bias_cor_method <- "none" # Options: "none", "median_unbiased", and "mean_unbiased"
em_bias_cor <- FALSE


# Case 1 : null case ------------------------------------------------------------------

null_case_input <- save_initial_input(base_case = TRUE, case_name = "C1")

## Run OM
run_om(input_list = null_case_input, show_iter_num = T)

# Case 2 : change logR_sd from 0.4 to 0.6 ------------------------------------------------------------------

updated_input <- save_initial_input(
  base_case = FALSE,
  input_list = null_case_input,
  case_name = "C2",
  logR_sd = 0.6
)
run_om(input_list = updated_input, show_iter_num = F)
```

## Disclaimer
“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”
