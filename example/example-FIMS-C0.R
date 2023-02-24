
# Operating model for FIMS first test case 0 where logR_sd = 0. All other OM
# settings are the same as case 0 in Li et al. 2021
# https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/fish-bull/11923li.pdf

# User will need to pick a "Set up" option below depending on their workflow...

# Set up OPTION 1: If user wants to use the ASSAMC package in their own project --------------------
# install.packages("remotes")
# install.packages("devtools")

# will put in a pull request when Bai gets back from leave, but unfortunately the NAMESPACE wasn't up
# to date (just need to run document()) on her spatial-structure branch, so I
# couldn't install the package properly.
remotes::install_github(repo="JaneSullivan-NOAA/Age_Structured_Stock_Assessment_Model_Comparison",
                        ref="spatial-structure")
library(ASSAMC)
maindir <- getwd() # or wherever you want your project rooted

# Set up OPTION 2: If user is developing within the ASSAMC package ------------------------------

# setwd() at project root if not there, e.g.,
# setwd("C:/Users/bai.li/Documents/Age_Structured_Stock_Assessment_Model_Comparison/")
devtools::load_all()
maindir <- paste0(getwd(), "/example")

# Basic simulation settings -------------------------------------------------------------------

om_sim_num <- 1 # total number of iterations per case
keep_sim_num <- 1 # number of kept iterations per case
figure_number <- 1 # number of individual iteration to plot

seed_num <- 9924

# Basic stock settings ------------------------------------------------------------------------------

year <- 1:30
num_stock <- 1
stocks <- vector(mode="list", length=num_stock)
names(stocks) <- paste("stock", 1:num_stock, sep="")


# Recruitment transportation probability settings ---------------------------------------------
recruit_transportation <- lapply(1:length(year),
                                 function(x)
                                   matrix(c(1),
                                          ncol=1, byrow=T))

# Movement Settings ---------------------------------------------------------------------
movement_matrix <- lapply(1:length(year),
                          function(x)
                            matrix(c(1),
                                   ncol=1, byrow=T))


# Life-history settings -----------------------------------------------------------------------

ages <- 1:12   #Age structure of the popn

initial_equilibrium_F <- TRUE
median_R0 <- 1000000 #Average annual unfished recruitment
median_h <- 0.75 #Steepness of the Beverton-Holt spawner-recruit relationship.
mean_R0 <- NULL
mean_h <- NULL
SRmodel <- 1 # 1=Beverton-Holt; 2=Ricker
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

# Biological reference points F vectors --------------------------------------------------------
brp_f_vector <- seq(0.0, 1, by=0.001)
brp_f_option <- "independentF" # options: independentF and dependentF
# brp_f_option <- "dependentF" # options: independentF and dependentF

#### Other settings ####
logf_sd <- 0.2
f_dev_change <- FALSE # for case 0, the OM creates the same set of F devs for a given iteration
f_pattern <- 1
start_val <- 0.01
middle_val <- NULL
end_val <- 0.39
f_val <- NULL
start_year <- 1
middle_year <- NULL

logR_sd <- 0 # new NULL case for FIMS where logR_sd = 0
r_dev_change <- TRUE

om_bias_cor <- FALSE
bias_cor_method <- "none" #Options: "none", "median_unbiased", and "mean_unbiased"
em_bias_cor <- FALSE

stocks$stock1 <- save_stock_input(base_stock=TRUE)

# Null case -----------------------------------------------------------------------------------

base_case_input <- save_initial_input(base_case = TRUE,
                                      input_list = stocks,
                                      case_name = "FIMS-C0")
# Run om
run_om(input_list=base_case_input, show_iter_num=T)


# Aggregate OM outputs ------------------------------------------------------------------------

om_list <- vector(mode="list", length=1)
load(file.path(maindir, "FIMS-C0", "output", "OM", paste("OM", 1, ".RData", sep="")))

for (j in 1:length(om_output$stocks)){
  load(file.path(maindir, "FIMS-C0", "output", "OM", paste("OM", 1, ".RData", sep="")))

  om_biomass <- om_abundance <-
    om_ssb <- om_recruit <- om_Ftot <- om_Fmul <-
    om_landing <- om_survey <- matrix(NA, nrow=om_input[[j]]$nyr, ncol=om_sim_num)

  om_msy <- om_fmsy <- om_ssbmsy <- matrix(NA, nrow=1, ncol=om_sim_num)

  om_fratio <- om_ssbratio <- matrix(NA, nrow=om_input[[j]]$nyr, ncol=om_sim_num)
  om_agecomp <- list()

  om_landing_err <- om_survey_err <- matrix(NA, nrow=om_input[[j]]$nyr, ncol=om_sim_num)
  om_geomR0 <- om_arimR0 <-
    om_geomS0 <- om_arimS0 <-
    om_geomDf <- om_arimDf <- matrix(NA, nrow=1, ncol=om_sim_num)

  for (om_sim in 1:om_sim_num){

    load(file.path(maindir, "FIMS-C0", "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    om_biomass[,om_sim] <- om_output$stocks[[j]]$biomass.mt
    om_abundance[,om_sim] <- om_output$stocks[[j]]$abundance/1000
    om_ssb[,om_sim] <- om_output$stocks[[j]]$SSB
    om_recruit[,om_sim] <- om_output$stocks[[j]]$N.age[,1]/1000
    om_Ftot[,om_sim] <- apply(om_output$stocks[[j]]$FAA, 1, max)
    om_landing[,om_sim] <- om_output$stocks[[j]]$L.mt$fleet1
    om_survey[,om_sim] <- om_output$stocks[[j]]$survey_index$survey1
    om_msy[, om_sim] <- om_output$stocks[[j]]$msy$msy
    om_fmsy[, om_sim] <- unique(round(om_output$stocks[[j]]$msy$Fmsy, digits = 3))
    om_ssbmsy[, om_sim] <- unique(om_output$stocks[[j]]$msy$SSBmsy)
    om_fratio[, om_sim] <- om_Ftot[, om_sim]/om_fmsy[om_sim]
    om_ssbratio[, om_sim] <- om_ssb[, om_sim]/om_ssbmsy[om_sim]
    om_agecomp[[om_sim]] <- apply(om_output$stocks[[j]]$N.age/1000, 1, function(x) x/sum(x))
    om_geomR0[,om_sim] <- om_input[[j]]$median_R0/1000
    om_arimR0[,om_sim] <- om_input[[j]]$mean_R0/1000
    om_geomS0[,om_sim] <- om_input[[j]]$median_R0*om_input[[j]]$Phi.0
    om_arimS0[,om_sim] <- om_input[[j]]$mean_R0*om_input[[j]]$Phi.0
    om_geomDf[,om_sim] <- om_ssb[nrow(om_ssb),om_sim]/om_geomS0[,om_sim]
    om_arimDf[,om_sim] <- om_ssb[nrow(om_ssb),om_sim]/om_arimS0[,om_sim]

  }

  om_list[[j]] <- list(om_biomass, om_abundance,
                       om_ssb, om_recruit, om_Ftot,
                       om_landing, om_survey,
                       om_msy, om_fmsy, om_ssbmsy,
                       om_fratio, om_ssbratio,
                       om_geomR0, om_arimR0,
                       om_geomS0, om_arimS0,
                       om_geomDf, om_arimDf,
                       om_agecomp)

  names(om_list[[j]]) <- c("biomass", "abundance",
                           "ssb", "recruit", "Ftot",
                           "landing", "survey",
                           "msy", "fmsy", "ssbmsy",
                           "fratio", "ssbratio",
                           "geomR0", "arimR0",
                           "geomS0", "arimS0",
                           "geomDf", "arimDf",
                           "agecomp")

}


# Plot ouputs ---------------------------------------------------------------------------------

var <- c("biomass", "abundance",
         "ssb", "recruit", "Ftot",
         "landing", "survey")
ylab <- c("Biomass", "Abundance",
          "SSB", "R", "F",
          "Landings", "Survey Index")

sim_id <- 1
for (i in 1:length(var)){
  ylim = range(om_list[[1]][[var[i]]][,sim_id])

  plot(0, type="n",
       xlab="Year",
       ylab=ylab[i],
       xlim=range(om_input$stock1$year),
       ylim=ylim)

  for (j in 1:length(om_list)){
    lines(om_input[[j]]$year, om_list[[j]][[var[i]]][,sim_id],
          col=j, type="o",
          pch=j, lty=j)
  }
  legend("topright",
         paste("Area", 1:length(om_output$stocks)),
         col=1:length(om_output$stocks),
         pch=1:length(om_output$stocks),
         lty=1:length(om_output$stocks),
         bty="n")
}


# Plot yield over F ---------------------------------------------------------------------------------
ylim = range(om_output$stocks$stock1$msy$L_eq)
xlim = c(0,2)

plot(om_output$stocks$stock1$msy$brp_f_vector,
     om_output$stocks$stock1$msy$L_eq,
     ylim=ylim, xlim=xlim,
     xlab="F", ylab="Yield",
     type="l", lty=1, col=1)

om_output$stocks$stock1$msy$Fmsy
which.max(om_output$stocks$stock1$msy$L_eq)

f_list <- vector(mode="list", length=length(om_output$stocks))
for (i in 1:length(f_list)){
  f_list[[i]] <- stocks[[i]]$brp_f_vector
}

f_combinations <- expand.grid(f_list)
data <- c(f_combinations,
          om_output$stocks$stock1$msy$L_eq)
sum_yield <- om_output$stocks$stock1$msy$L_eq
which.max(sum_yield)
f_combinations[which.max(sum_yield),]
