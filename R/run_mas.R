#' Function to run Metapopulation Assessment System (https://github.com/nmfs-fish-tools/r4MAS)
#' @name run_mas
#' @description Function to read simulated true values from the operating model and to run MAS
#' @param maindir Main working directory
#' @param subdir Estimation model working directory
#' @param om_sim_num Number of iterations from the operating model
#' @param casedir Case working directory
#' @export
run_mas <- function(maindir = maindir, subdir = "MAS", om_sim_num = NULL, casedir = casedir) {
  if (!("r4MAS" %in% installed.packages()[, "Package"])) stop("Please install r4MAS!")

  setwd(file.path(casedir, "output", subdir))
  unlink(list.files(file.path(casedir, "output", "MAS"), full.names = TRUE), recursive = TRUE)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep = ""))))

  for (om_sim in 1:om_sim_num) {
    load(file=file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    library(r4MAS)
    r4mas <- Rcpp::Module("rmas", PACKAGE = "r4MAS")

    nyears <- om_input$nyr
    nseasons <- 1
    nages <- om_input$nages
    ages <- om_input$ages
    area1 <- new(r4mas$Area)
    area1$name <- "area1"

    recruitment <- new(r4mas$BevertonHoltRecruitment)
    recruitment$R0$value <- om_input$R0 / 1000
    recruitment$R0$estimated <- TRUE
    recruitment$R0$phase <- 1
    recruitment$h$value <- om_input$h
    recruitment$h$estimated <- FALSE
    recruitment$h$phase <- 3
    recruitment$h$min <- 0.2001
    recruitment$h$max <- 1.0
    recruitment$sigma_r$value <- om_input$logR_sd
    recruitment$sigma_r$estimated <- FALSE
    recruitment$sigma_r$min <- 0
    recruitment$sigma_r$max <- 1.0
    recruitment$sigma_r$phase <- 2
    recruitment$estimate_deviations <- TRUE
    recruitment$constrained_deviations <- TRUE
    recruitment$deviations_min <- -15.0
    recruitment$deviations_max <- 15.0
    recruitment$deviation_phase <- 2
    recruitment$SetDeviations(om_input$logR.resid)
    recruitment$use_bias_correction <- FALSE

    growth <- new(r4mas$VonBertalanffyModified)
    empirical_weight <- rep(om_input$W.kg, times = om_input$nyr)
    survey_empirical_weight <- replicate(nages * nyears, 1.0)
    growth$SetUndifferentiatedCatchWeight(empirical_weight)
    growth$SetUndifferentiatedWeightAtSeasonStart(empirical_weight)
    growth$SetUndifferentiatedWeightAtSpawning(empirical_weight)
    growth$SetUndifferentiatedSurveyWeight(survey_empirical_weight)

    maturity <- new(r4mas$Maturity)
    maturity$values <- om_input$mat.age * 0.5

    natural_mortality <- new(r4mas$NaturalMortality)
    natural_mortality$SetValues(om_input$M.age)

    # Only 1 area in this model
    movement <- new(r4mas$Movement)
    movement$connectivity_females <- c(0.0)
    movement$connectivity_males <- c(0.0)
    movement$connectivity_recruits <- c(0.0)

    initial_deviations <- new(r4mas$InitialDeviations)
    initial_deviations$values <- rep(0.0, times = om_input$nages)
    initial_deviations$estimate <- TRUE
    initial_deviations$phase <- 2

    population <- new(r4mas$Population)
    for (y in 1:(nyears))
    {
      population$AddMovement(movement$id, y)
    }
    population$AddNaturalMortality(natural_mortality$id, area1$id, "undifferentiated")
    population$AddMaturity(maturity$id, area1$id, "undifferentiated")
    population$AddRecruitment(recruitment$id, 1, area1$id)
    population$SetInitialDeviations(initial_deviations$id, area1$id, "undifferentiated")
    population$SetGrowth(growth$id)
    population$sex_ratio <- 0.5

    # Catch index values and observation errors
    catch_index <- new(r4mas$IndexData)
    catch_index$values <- em_input$L.obs$fleet1
    catch_index$error <- rep(em_input$cv.L$fleet1, times = om_input$nyr)
    # Catch composition data
    catch_comp <- new(r4mas$AgeCompData)
    catch_comp$values <- as.vector(t(em_input$L.age.obs$fleet1))
    catch_comp$sample_size <- rep(em_input$n.L$fleet1, nyears * nseasons)
    # Likelihood component settings
    fleet_index_comp_nll <- new(r4mas$Lognormal)
    fleet_index_comp_nll$use_bias_correction <- FALSE
    fleet_age_comp_nll <- new(r4mas$Multinomial)
    # Fleet selectivity settings
    fleet_selectivity <- new(r4mas$LogisticSelectivity)
    fleet_selectivity$a50$value <- om_input$sel_fleet$fleet1$A50.sel
    fleet_selectivity$a50$estimated <- TRUE
    fleet_selectivity$a50$phase <- 2
    fleet_selectivity$a50$min <- 0.0
    fleet_selectivity$a50$max <- max(om_input$ages)
    fleet_selectivity$slope$value <- 1 / om_input$sel_fleet$fleet1$slope.sel
    fleet_selectivity$slope$estimated <- TRUE
    fleet_selectivity$slope$phase <- 2
    fleet_selectivity$slope$min <- 0.0001
    fleet_selectivity$slope$max <- 5
    # Fishing mortality settings
    fishing_mortality <- new(r4mas$FishingMortality)
    fishing_mortality$estimate <- TRUE
    fishing_mortality$phase <- 1
    fishing_mortality$min <- 0.0
    fishing_mortality$max <- 4
    fishing_mortality$SetValues(om_output$f)
    # Create the fleet
    fleet <- new(r4mas$Fleet)
    fleet$AddIndexData(catch_index$id, "undifferentiated")
    fleet$AddAgeCompData(catch_comp$id, "undifferentiated")
    fleet$SetIndexNllComponent(fleet_index_comp_nll$id)
    fleet$SetAgeCompNllComponent(fleet_age_comp_nll$id)
    fleet$AddSelectivity(fleet_selectivity$id, 1, area1$id)
    fleet$AddFishingMortality(fishing_mortality$id, 1, area1$id)

    # Survey index values and observation errors
    survey_index <- new(r4mas$IndexData)
    survey_index$values <- em_input$survey.obs$survey1
    survey_index$error <- rep(em_input$cv.survey$survey1, times = om_input$nyr)
    # Survey composition
    survey_comp <- new(r4mas$AgeCompData)
    survey_comp$values <- as.vector(t(em_input$survey.age.obs$survey1))
    survey_comp$sample_size <- rep(em_input$n.survey$survey1, times = om_input$nyr)
    # Likelihood component settings
    survey_index_comp_nll <- new(r4mas$Lognormal)
    survey_index_comp_nll$use_bias_correction <- FALSE
    survey_age_comp_nll <- new(r4mas$Multinomial)
    # Survey selectivity settings
    survey_selectivity <- new(r4mas$LogisticSelectivity)
    survey_selectivity$a50$value <- om_input$sel_survey$survey1$A50.sel
    survey_selectivity$a50$estimated <- TRUE
    survey_selectivity$a50$phase <- 2
    survey_selectivity$a50$min <- 0
    survey_selectivity$a50$max <- max(om_input$ages)
    survey_selectivity$slope$value <- 1 / om_input$sel_survey$survey1$slope.sel
    survey_selectivity$slope$estimated <- TRUE
    survey_selectivity$slope$phase <- 2
    survey_selectivity$slope$min <- 0.0001
    survey_selectivity$slope$max <- 5
    # Create the survey
    survey <- new(r4mas$Survey)
    survey$AddIndexData(survey_index$id, "undifferentiated")
    survey$AddAgeCompData(survey_comp$id, "undifferentiated")
    survey$SetIndexNllComponent(survey_index_comp_nll$id)
    survey$SetAgeCompNllComponent(survey_age_comp_nll$id)
    survey$AddSelectivity(survey_selectivity$id, 1, area1$id)
    # Catchability settings
    survey$q$value <- em_input$survey_q$survey1
    survey$q$min <- 0
    survey$q$max <- 10
    survey$q$estimated <- TRUE
    survey$q$phase <- 1

    mas_model <- new(r4mas$MASModel)
    mas_model$nyears <- nyears
    mas_model$nseasons <- nseasons
    mas_model$nages <- nages
    mas_model$extended_plus_group <- max(om_input$ages)
    mas_model$ages <- ages
    mas_model$catch_season_offset <- 0.0
    mas_model$spawning_season_offset <- 0.0
    mas_model$survey_season_offset <- 0.0
    mas_model$AddPopulation(population$id)
    mas_model$AddFleet(fleet$id)
    mas_model$AddSurvey(survey$id)
    # mas_model$tolerance <- 0.0001

    # Run MAS
    mas_model$Run()
    output_file <- file.path(casedir, "output", subdir, paste("s", om_sim, sep = ""), paste("s", om_sim, ".json", sep = ""))
    write(mas_model$GetOutput(), file = toString(output_file))
    mas_model$Reset()
  }
}
