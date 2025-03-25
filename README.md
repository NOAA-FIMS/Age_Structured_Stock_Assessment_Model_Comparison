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
remotes::install_github("NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison")
```

## Usage
```r
# Set up project directory ---------------------------------------------------------------
project_dir <- "XXX"

## Download and keep Age_Structured_Stock_Assessment_Model_Comparison/example/em_input/ folder in the project directory

# Set up OM ---------------------------------------------------------------

# Use the example input lists for case one and case two called
# null_case_input and updated_input from the following example
example(save_initial_input)
# Case 1 : null case ----------------------------------------------------------
run_om(input_list = null_case_input, show_iter_num = TRUE)

# Case 2 : change logR_sd from 0.4 to 0.6 -------------------------------------
run_om(input_list = updated_input, show_iter_num = FALSE)
```

## Disclaimer
“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”
