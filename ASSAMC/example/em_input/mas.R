setwd("C:/Users/bai.li/Documents/Github/RMAS-master")
library(Rcpp)
library(jsonlite)
library(callr)
#Load the RMAS package - assumes you are in the package working directory
devtools::load_all(".")

#load("OM Outputs/OM1.RData")
#output_file<- "EM_Output.json"
i=1

str<- paste(c("test/OM/OM",i), collapse = "")
str2<- paste(c(str,".RData"), collapse = "")
load(str2)

ostr<- paste(c("MAS/s",i), collapse = "")
output_file<- paste(c(ostr,".json"), collapse = "")



#load the r4mas module
r4mas <-
  Module("rmas", dyn.load(paste("src/RMAS", .Platform$dynlib.ext, sep = "")))




# nyears<-30
nyears<-om_input$nyr #
nseasons<-1
# nages<-12
nages<-om_input$nages #

#ages<-c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
ages <- om_input$ages # from 1 to 12
#How does the SSB and R been modeled in MAS? If it is similar with SS [Rt = f(SSBt)], then extra work may needed for MAS to set up the ages. If it is a standard relationship [Rt+1 = f(SSBt)], then ages could be from 1 to 12.

#define area
area1<-new(r4mas$Area)
area1$name<-"area1"

#Recruitment
recruitment<-new(r4mas$BevertonHoltRecruitment)
# recruitment$R0$value<-1000
recruitment$R0$value<-om_input$R0/1000 #
recruitment$R0$estimated<-sample(c(TRUE,FALSE), 1, TRUE)
recruitment$R0$phase<-1
# recruitment$h$value<-0.75
recruitment$h$value<-om_input$h #
recruitment$h$estimated<-FALSE
# recruitment$h$phase<-2
recruitment$h$phase<-3 # changed from 2 to 3 to match setting in other models, but it doesn't affect the results because h is not estimated
recruitment$h$min<-0.02001
recruitment$h$max<-1.0
# recruitment$sigma_r$value<-0.005545130293761911
# print(par.sim1$logR.sd)
recruitment$sigma_r$value <- exp(om_input$logR_sd) #0.2 #par.sim1$logR.sd # if simgma_r is the SD of recruitment in log space
recruitment$sigma_r$estimated<-FALSE
recruitment$sigma_r$min<-0
recruitment$sigma_r$max<-1.0
recruitment$sigma_r$phase<-2
recruitment$estimate_deviations<-sample(c(TRUE,FALSE), 1, TRUE)
recruitment$constrained_deviations<-FALSE
recruitment$deviations_min<--15.0
recruitment$deviations_max<-15.0
recruitment$deviation_phase<-1
recruitment$SetDeviations(rep(0.0, times=om_input$nyr)) #year 1 to 30?

#Growth
growth<-new(r4mas$VonBertalanffyModified)
empirical_weight<-rep(om_input$W.kg, times=om_input$nyr) #
print(empirical_weight)
survey_empirical_weight<-replicate(nages*nyears, 1.0)
length(survey_empirical_weight)
growth$SetUndifferentiatedCatchWeight(empirical_weight)
growth$SetUndifferentiatedSurveyWeight(survey_empirical_weight)
growth$SetUndifferentiatedWeightAtSeasonStart(empirical_weight)
growth$SetUndifferentiatedWeightAtSpawning(empirical_weight)
growth$a_min$value<-0.01
growth$a_max$value<-10.0
growth$c$value<-0.3
growth$lmin$value<-5
growth$lmax$value<-50
growth$alpha_f$value<-0.000025
growth$alpha_m$value<-0.000025
growth$beta_f$value<-2.9624
growth$beta_m$value<-2.9624
#I assume that growth is not used in MAS and empirical?weight-at-age data are used in this case, so I did not modify the growth part. The true growth and weight-at-length in the OM is La=Linf*(1-exp(-K(a-t0)) and Wa=a*La^b/1000. Linf=800 mm, K=0.18, t0=-1.36, a=2.5e-8, b=3.

#Maturity
maturity<-new(r4mas$Maturity)
maturity$values <- om_input$mat.age*.5 # If sex ratio is defined somewhere. If not, then maturity$values <- par.sim1$mat.age/2

#Natural Mortality
natural_mortality<-new(r4mas$NaturalMortality)
natural_mortality$SetValues(om_input$M.age) #

#define Movement (only 1 area in this model)
movement<-new(r4mas$Movement)
movement$connectivity_females<-c(1.0)
movement$connectivity_males<-c(1.0)
movement$connectivity_recruits<-c(1.0)

#Initial Deviations
initial_deviations<-new(r4mas$InitialDeviations)
initial_deviations$values<-rep(0, times=om_input$nages) #
#I assume initial deviations are the number at age in year 1 deviations.
#For ASAP and BAM, they estimate deviations from age 2 to 12 in year 1.
#For AMAK and SS, they use the ramp function to estimate deviations from age 1 to 12. What about MAS?
initial_deviations$estimate<-FALSE #
initial_deviations$phase<-1

population<-new(r4mas$Population)
show(r4mas$Population)
for (y in 0:(nyears))
{
  population$AddMovement(movement$id, y)
}

population$AddNaturalMortality(natural_mortality$id,area1$id,"undifferentiated")
population$AddMaturity(maturity$id,area1$id, "undifferentiated")
population$AddRecruitment(recruitment$id,1,area1$id)
population$SetInitialDeviations(initial_deviations$id, area1$id, "undifferentiated")
population$SetGrowth(growth$id)
population$sex_ratio<-0.5

#Fishing Mortality
fishing_mortality<-new(r4mas$FishingMortality)
fishing_mortality$estimate<-TRUE
fishing_mortality$phase<-1
fishing_mortality$min<-0.0
fishing_mortality$max<-4 # For matching settings in other models
fishing_mortality$SetValues(om_output$f) #For the model comparison project, true values are used for initial guess

#Selectivity Model

fleet_selectivity<-new(r4mas$LogisticSelectivity)
fleet_selectivity$a50$value<-om_input$sel_fleet$fleet1$A50.sel #
fleet_selectivity$a50$estimated<-sample(c(TRUE,FALSE), 1, TRUE) # S2 is selected as base case scenario, and the only difference between S1 and S2 are estimating selectivity parameters or not
fleet_selectivity$a50$phase<-2 #
fleet_selectivity$a50$min<-0.0 #
fleet_selectivity$a50$max<-max(par.sim1$ages) #

fleet_selectivity$slope$value<-1/om_input$sel_fleet$fleet1$slope.sel  #
fleet_selectivity$slope$estimated<-fleet_selectivity$a50$estimated #
fleet_selectivity$slope$phase<-2 #
fleet_selectivity$slope$min<-0 #
fleet_selectivity$slope$max<-max(om_input$ages) #

survey_selectivity<-new(r4mas$LogisticSelectivity)
survey_selectivity$a50$value<-om_input$sel_survey$survey1$A50.sel #
survey_selectivity$a50$estimated<-sample(c(TRUE,FALSE), 1, TRUE) #
survey_selectivity$a50$phase<-2 #
survey_selectivity$a50$min<-0.0 #
survey_selectivity$a50$max<-max(om_input$ages) #

survey_selectivity$slope$value<-1/om_input$sel_survey$survey1$slope.sel #
survey_selectivity$slope$estimated<-survey_selectivity$a50$estimated #
survey_selectivity$slope$phase<-2 #
survey_selectivity$slope$min<-0 #
survey_selectivity$slope$max<-max(om_input$ages) #

#Index data
catch_index<-new(r4mas$IndexData)
catch_index$values<-em_input$L.obs$fleet1 #
#dat.input$cv.L
catch_index$error<-rep(em_input$cv.L$fleet1, times=om_input$nyr) #

survey_index<-new(r4mas$IndexData)
survey_index$values<-em_input$survey.obs$survey1 #
survey_index$error<-rep(em_input$cv.survey$survey1, times=om_input$nyr) #


#Age Comp Data
catch_comp<-new(r4mas$AgeCompData)
catch_comp$values<-as.vector(t(em_input$L.age.obs$fleet1)) #
catch_comp$sample_size<-rep(em_input$n.L$fleet1, times=om_input$nages) #

survey_comp<-new(r4mas$AgeCompData)
survey_comp$values<-as.vector(t(em_input$survey.age.obs$survey1)) #
survey_comp$sample_size<-rep(em_input$n.survey$survey1, times=om_input$nyr) #

#NLL models
fleet_index_comp_nll<-new(r4mas$Lognormal)
fleet_age_comp_nll<-new(r4mas$Multinomial)
#The other four models uses standard multinomial for age comp nll
survey_index_comp_nll<-new(r4mas$Lognormal)
survey_age_comp_nll<-new(r4mas$Multinomial)
#The other four models uses standard multinomial for age comp nll


#Fleet
fleet<-new(r4mas$Fleet)
fleet$AddAgeCompData(catch_comp$id, "undifferentiated")
fleet$AddIndexData(catch_index$id, "undifferentiated")
fleet$SetAgeCompNllComponent(fleet_age_comp_nll$id)
fleet$SetIndexNllComponent(fleet_index_comp_nll$id)
fleet$AddSelectivity(fleet_selectivity$id, 1, area1$id)
fleet$AddFishingMortality(fishing_mortality$id, 1,area1$id)


#Survey
survey<-new(r4mas$Survey)
survey$AddAgeCompData(survey_comp$id,"undifferentiated")
survey$AddIndexData(survey_index$id,"undifferentiated")
survey$SetIndexNllComponent(survey_index_comp_nll$id)
survey$SetAgeCompNllComponent(survey_age_comp_nll$id)
survey$AddSelectivity(survey_selectivity$id, 1, area1$id)
survey$q$value<-em_input$survey_q$survey1 #
survey$q$min<-0
survey$q$max<-10 #
survey$q$estimated<-TRUE
survey$q$phase<-1



#build the MAS model
mas_model<-new(r4mas$MASModel)
mas_model$nyears<-nyears
mas_model$nseasons<-nseasons
mas_model$nages<-nages
#mas_model$extended_plus_group<-15
mas_model$extended_plus_group<-max(om_input$ages) #not sure about this, 12+ is the plus group in the OM
mas_model$ages<-ages
mas_model$AddFleet(fleet$id)
mas_model$catch_season_offset<-0.0
mas_model$spawning_season_offset<-0.0 #Jan-1
mas_model$survey_season_offset<-0.0
#survey time is also Jan-1, I don't know if it needs to be defined in MAS

mas_model$AddSurvey(survey$id)
mas_model$AddPopulation(population$id)
# print(mas_model$GetJSONConfig())
mas_model$Run()
write(mas_model$GetOutput(), file=toString(output_file))
mas_model$Reset()


#read output



args <- toString(output_file)
json_output <- read_json(args[1])
popdy<-json_output$population_dynamics
years <- popdy$nyears
seasons<-popdy$nseasons
areas<-popdy$nareas
ages_ <- popdy$nages

#population
pop<-popdy$populations[[1]]
flt<-popdy$fleets[[1]]
srvy<-popdy$surveys[[1]]
area<-popdy$areas[[1]]
pop_naa<-pop$undifferentiated$numbers_at_age
pop_recruits<-pop$undifferentiated$recruits
pop_abundance<-pop$undifferentiated$abundance
pop_abundance_f<-pop$females$abundance
pop_abundance_m<-pop$males$abundance
pop_ssb<-pop$undifferentiated$spawning_stock_biomass
pop_biomass<-pop$undifferentiated$biomass
pop_F<-pop$undifferentiated$fishing_mortality
flt_landings<-flt$undifferentiated$catch_biomass
srvy_landings<-srvy$undifferentiated$survey_biomass
srvy_props<-srvy$undifferentiated$survey_proportion_at_age



# print(x)
postr<- paste(c("EM Outputs/Fig",i), collapse = "")
plot_file<- "Fig_true_estimate_timeseries.jpg"
jpeg(toString(plot_file), width = 6, height = 4, units = 'in', res = 300)
x<-seq(1,30, 1)
par(mfrow=c(2,4))
plot( x, unlist(pop_biomass$values), type="l", col="red"
      ,ylab="Biomass (mt)")
lines(x, sim1$biomass.mt,type="p")

plot( x,unlist(pop_abundance$values), type="l", col="red"
      ,ylab="Abundance (1000 fish)")
lines(x, sim1$abundance/1000,type="p")


plot( x,sim1$SSB,type="p"
      ,ylab="SSB (mt)")
lines(x,unlist(pop_ssb$values) ,type="l", col="red")

#
plot( x,unlist(pop_recruits$values), type="l", col="red"
      ,ylab="Recruitment (1000 fish)")
lines(x, sim1$N.age[,1]/1000,type="p")



plot( x,sim1$F, type="p"
      ,ylab="F")
lines(x, unlist(pop_F$values),type="l",col="red")


plot( x,unlist(flt_landings$values), type="l", col="red"
      ,ylab="Landings (mt)")
lines(x, sim1$L.mt,type="p")

plot( x,unlist(srvy_landings$values), type="l", col="red"
      ,ylab="Survey Index")
lines(x, survey.sim1,type="p")