
# Library packages ----------------------------------------------------------------------------

# install.packages("remotes")
# install.packages("devtools")
# remotes::install_github(repo="NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison",
#                         ref="spatial-structure")
# library(ASSAMC)
#
setwd("C:/Users/bai.li/Documents/Age_Structured_Stock_Assessment_Model_Comparison/")
devtools::load_all()

# Working directory settings ------------------------------------------------------------------

maindir <- "C:/Users/bai.li/Documents/Age_Structured_Stock_Assessment_Model_Comparison/example"

# Basic simulation settings -------------------------------------------------------------------

om_sim_num <- 1 # total number of iterations per case
keep_sim_num <- 1 # number of kept iterations per case
figure_number <- 1 # number of individual iteration to plot

seed_num <- 9924

# Basic stock settings ------------------------------------------------------------------------------

year <- 1:30
num_stock <- 3
stocks <- vector(mode="list", length=num_stock)
names(stocks) <- paste("stock", 1:num_stock, sep="")


# Recruitment transportation probability settings ---------------------------------------------
recruit_transportation <- lapply(1:length(year),
                                 function(x)
                                   matrix(c(1, 1, 1),
                                          ncol=3, byrow=T))
# recruit_transportation <- lapply(1:length(year),
#                                  function(x)
#                                    matrix(c(0.68, 0.22, 0.10),
#                                           ncol=3, byrow=T))


# Movement Settings ---------------------------------------------------------------------
movement_matrix <- lapply(1:length(year),
                          function(x)
                            matrix(c(0.68, 0.22, 0.10,
                                     0.24, 0.37, 0.39,
                                     0.08, 0.28, 0.64),
                                   ncol=3, byrow=T))


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



# Bilogical reference points F vectors --------------------------------------------------------
brp_f_vector <- seq(0.0, 1, by=0.001)
brp_f_option <- "independentF" # options: independentF and dependentF
# brp_f_option <- "dependentF" # options: independentF and dependentF

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

stocks$stock1 <- save_stock_input(base_stock=TRUE)

# Reset fleet selectivity
sel_fleet <- list()
sel_fleet$fleet1$pattern <- 1
sel_fleet$fleet1$A50.sel1 <- 4
sel_fleet$fleet1$slope.sel1 <- 2
stocks$stock2 <- save_stock_input(base_stock=FALSE,
                                  input_list=stocks$stock1,
                                  median_h=0.8,
                                  f_pattern=5,
                                  start_val=0.01,
                                  middle_val=0.6,
                                  end_val=NULL,
                                  f_val=NULL,
                                  start_year=1,
                                  middle_year=6)

# Reset fleet selectivity
stocks$stock3 <- save_stock_input(base_stock=FALSE,
                                  input_list=stocks$stock1,
                                  median_h=0.7)


# Null case -----------------------------------------------------------------------------------

base_case_input <- save_initial_input(base_case = TRUE,
                                      input_list = stocks,
                                      case_name = "C0")
# Run om
run_om(input_list=base_case_input, show_iter_num=T)


# Aggregate OM outputs ------------------------------------------------------------------------

om_list <- vector(mode="list", length=3)

for (j in 1:length(om_output$stocks)){
  load(file.path(maindir, "C0", "output", "OM", paste("OM", 1, ".RData", sep="")))

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

    load(file.path(maindir, "C0", "output", "OM", paste("OM", om_sim, ".RData", sep="")))

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
  ylim = range(om_list[[1]][[var[i]]][,sim_id],
               om_list[[2]][[var[i]]][,sim_id],
               om_list[[3]][[var[i]]][,sim_id])

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
ylim = range(om_output$stocks$stock1$msy$L_eq,
             om_output$stocks$stock2$msy$L_eq,
             om_output$stocks$stock3$msy$L_eq)
xlim = c(0,2)

plot(om_output$stocks$stock1$msy$brp_f_vector,
     om_output$stocks$stock1$msy$L_eq,
     ylim=ylim, xlim=xlim,
     xlab="F", ylab="Yield",
     type="l", lty=1, col=1)
lines(om_output$stocks$stock2$msy$brp_f_vector,
      om_output$stocks$stock2$msy$L_eq,
      lty=2, col=2)
lines(om_output$stocks$stock3$msy$brp_f_vector,
      om_output$stocks$stock3$msy$L_eq,
      lty=3, col=3)

om_output$stocks$stock1$msy$Fmsy
om_output$stocks$stock2$msy$Fmsy
om_output$stocks$stock3$msy$Fmsy
which.max(om_output$stocks$stock1$msy$L_eq)
which.max(om_output$stocks$stock2$msy$L_eq)
which.max(om_output$stocks$stock3$msy$L_eq)

f_list <- vector(mode="list", length=length(om_output$stocks))
for (i in 1:length(f_list)){
  f_list[[i]] <- stocks[[i]]$brp_f_vector
}

f_combinations <- expand.grid(f_list)
data <- c(f_combinations,
          om_output$stocks$stock1$msy$L_eq,
          om_output$stocks$stock2$msy$L_eq,
          om_output$stocks$stock3$msy$L_eq)
sum_yield <- om_output$stocks$stock1$msy$L_eq+
  om_output$stocks$stock2$msy$L_eq+
  om_output$stocks$stock3$msy$L_eq
which.max(sum_yield)
f_combinations[which.max(sum_yield),]

