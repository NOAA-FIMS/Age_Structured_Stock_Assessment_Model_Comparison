# install.packages("remotes")
# install.packages("devtools")
# remotes::install_github(repo="Bai-Li-NOAA/Age_Structured_Stock_Assessment_Model_Comparison", ref="full-features", build_vignettes=T)
# library(ASSAMC)

setwd("C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/")
devtools::load_all()
## Need to install packages below:
## ASAPplots, r4ss, readxl, PBSadmb
#devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)
library(readxl)
library(PBSadmb)
library(ASAPplots)
library(r4ss)

## Setup working directory
maindir <- "C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/bias_adjustment"

om_sim_num <- 160 # total number of iterations per case
keep_sim_num <- 100 # number of kept iterations per case
figure_number <- 10 # number of individual iteration to plot

seed_num <- 9924

#### Life-history parameters ####
year <- 1:30
ages <- 1:12   #Age structure of the popn

initial_equilibrium_F <- TRUE
median_R0 <- 1000000 #Average annual unfished recruitment (scales the popn)
median_h <- 0.75 #Steepness of the Beverton-Holt spawner-recruit relationship.
mean_R0 <- NULL
mean_h <- NULL
SRmodel <- 2 # 1=Beverton-Holt; 2=Ricker
M <- 0.2       #Age-invariant natural mortality

Linf <- 800	  #Asymptotic average length
K <- 0.18     	#Growth coefficient
a0 <- -1.36    #Theoretical age at size 0
a.lw <- 0.000000025  #Length-weight coefficient
b.lw <- 3.0     	    #Length-weight exponent
A50.mat <- 2.25      #Age at 50% maturity
slope.mat <- 3       #Slope of maturity ogive
pattern.mat <- 1     #Simple logistic maturity
female.proportion <- 0.5   #Sex ratio

#### Fleet settings ####
fleet_num <- 1

#CV of landings for OM
cv.L <- list()
cv.L$fleet1 <- 0.005

#Input CV of landings for EMs
input.cv.L <- list()
input.cv.L$fleet1 <- 0.01

#Annual sample size (nfish) of age comp samples
n.L <- list()
n.L$fleet1 <- 200

#Define fleet selectivity
sel_fleet <- list()

sel_fleet$fleet1$pattern <- 1
sel_fleet$fleet1$A50.sel1 <- 2
sel_fleet$fleet1$slope.sel1 <- 1

#### Survey settings ####
survey_num <- 1

#CV of surveys for OM
cv.survey <- list()
cv.survey$survey1 <- 0.1

#Input CV of surveys for EMs
input.cv.survey <- list()
input.cv.survey$survey1 <- 0.2

#Annual sample size (nfish) of age comp samples
n.survey <- list()
n.survey$survey1 <- 200

#Define survey selectivity
sel_survey <- list()

sel_survey$survey1$pattern <- 1
sel_survey$survey1$A50.sel1 <- 1.5
sel_survey$survey1$slope.sel1 <- 2

#### Other settings ####
logf_sd <- 0.2
f_dev_change <- FALSE
f_pattern <- 1
start_val <- 0.01
middle_val <- NULL
end_val <- 0.39
f_val <- NULL
start_year <- 1
middle_year <- NULL

logR_sd <- 0.6
r_dev_change <- TRUE

om_bias_cor <- FALSE
bias_cor_method <- "none" #Options: "none", "median_unbiased", and "mean_unbiased"
em_bias_cor <- FALSE

base_case_input <- save_initial_input(base_case=TRUE, case_name = "C0")
#### Base Case (Case 0) ####
run_om(input_list=base_case_input, show_iter_num=F)
run_em(em_names=c("BAM", "SS"), input_list=base_case_input)
generate_plot(em_names = c("BAM", "SS"), plot_ncol=2, plot_nrow=1, plot_color = c("red", "deepskyblue3"), input_list=base_case_input)

#### Case 1 (EM bias adjustment=TRUE but no conversion of h for inputs) ####
updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=base_case_input,
                                    case_name="C1",
                                    om_bias_cor=TRUE,
                                    bias_cor_method="median_unbiased",
                                    em_bias_cor=TRUE)
run_om(input_list=updated_input , show_iter_num=F)
run_em(em_names=c("BAM", "SS"), input_list=updated_input)
updated_input$keep_sim_num=80
generate_plot(em_names = c("BAM", "SS"),
              plot_ncol=2, plot_nrow=1,
              plot_color = c("red", "deepskyblue3"),
              input_list=updated_input)

#### Case 2 (EM bias adjustment=TRUE and convert h for inputs) ####
updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=base_case_input,
                                    case_name="C2",
                                    om_bias_cor=TRUE,
                                    bias_cor_method="median_unbiased",
                                    em_bias_cor=TRUE)
run_om(input_list=updated_input , show_iter_num=F)
run_em(em_names=c("BAM", "SS"), input_list=updated_input)
updated_input$keep_sim_num=80
generate_plot(em_names = c("BAM", "SS"),
              plot_ncol=2, plot_nrow=1,
              plot_color = c("red", "deepskyblue3"),
              input_list=updated_input)

#### Case 3 ####
updated_input <- save_initial_input(base_case=FALSE,
                                    input_list=base_case_input,
                                    case_name="C3",
                                    om_bias_cor=TRUE,
                                    bias_cor_method="mean_unbiased",
                                    em_bias_cor=TRUE)
run_om(input_list=updated_input , show_iter_num=F)
run_em(em_names=c("BAM", "SS"), input_list=updated_input)
generate_plot(em_names = c("BAM", "SS"),
              plot_ncol=2, plot_nrow=1,
              plot_color = c("red", "deepskyblue3"),
              input_list=updated_input)
