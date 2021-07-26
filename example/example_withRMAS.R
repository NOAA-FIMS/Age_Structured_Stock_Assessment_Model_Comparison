rmas_dir <- "C:/Users/bai.li/Documents/Github/RMAS-master/src/"
devtools::load_all(rmas_dir)

setwd("C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/")
#setwd("C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/")
devtools::load_all()
## Need to install packages below:
## ASAPplots, r4ss, readxl, RMAS
maindir <- "C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/example"

om_sim_num <- 160 # total number of iterations per case
keep_sim_num <- 100 # number of kept iterations per case
figure_number <- 10 # number of individual iteration to plot

seed_num <- 9924

year <- 1:30

logf_sd <- 0.2
f_dev_change <- FALSE
f_pattern <- 1
f_min <- 0.01
f_max <- 0.39

logR_sd <- 0.2
r_dev_change <- TRUE

em_bias_cor <- FALSE

#### Life-history parameters ####
ages=1:12           #Age structure of the popn

R0=1000000  #Average annual unfished recruitment (scales the popn)
h=0.75	    #Steepness of the Beverton-Holt spawner-recruit relationship.
M=0.2       #Age-invariant natural mortality

Linf=800	  #Asymptotic average length
K=0.18     	#Growth coefficient
a0=-1.36    #Theoretical age at size 0
a.lw=0.000000025  #Length-weight coefficient
b.lw=3.0     	    #Length-weight exponent
A50.mat=2.25      #Age at 50% maturity
slope.mat=3       #Slope of maturity ogive
pattern.mat=1     #Simple logistic maturity
female.proportion=0.5   #Sex ratio

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
sel_fleet$fleet1$A50.sel <- 2
sel_fleet$fleet1$slope.sel <- 1


#### Survey settings ####
survey_num <- 1

#CV of surveys for OM
cv.survey <- list()
cv.survey$survey1 <- 0.1
#cv.survey$survey2 <- 0.1

#Input CV of surveys for EMs
input.cv.survey <- list()
input.cv.survey$survey1 <- 0.2
#input.cv.survey$survey2 <- 0.2

#Annual sample size (nfish) of age comp samples
n.survey <- list()
n.survey$survey1 <- 200
#n.survey$survey2 <- 200

#Define survey selectivity
sel_survey <- list()

sel_survey$survey1$pattern <- 1
sel_survey$survey1$A50.sel <- 1.5
sel_survey$survey1$slope.sel <- 2

#sel_survey$survey2$pattern <- 1
#sel_survey$survey2$A50.sel <- 1.5
#sel_survey$survey2$slope.sel <- 2


#### Base Case ####
#### Run OM
run_om(maindir=maindir)

#### Run EMs
# run_em(run_em_names=c("AMAK", "ASAP"))
# run_em(run_em_names=c("BAM"))
# run_em(run_em_names=c("SS"))
run_em(run_em_names=c("MAS"))

#### Plot comparison outputs
generate_plot(em_names = c("MAS"), plot_ncol=1, plot_nrow=1, plot_color = c("orange"))

# generate_plot(em_names = c("AMAK", "ASAP", "BAM", "SS"), plot_ncol=2, plot_nrow=2, plot_color = c("orange", "green", "red", "deepskyblue3"))
#
# generate_plot(em_names = c("AMAK", "ASAP", "BAM"), plot_ncol=3, plot_nrow=1, plot_color = c("orange", "green", "red"))

#### Case 1 ####
logR_sd <- 0.4
run_om(maindir=maindir)
run_em(run_em_names=c("MAS"))
generate_plot(em_names = c("MAS"), plot_ncol=1, plot_nrow=1, plot_color = c("orange"))
