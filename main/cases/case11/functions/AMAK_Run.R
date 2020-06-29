AMAK_Run <- function(maindir=maindir, subdir="AMAK", om_sim_num=1){
  setwd(file.path(maindir, "output", subdir))
  sapply(1:om_sim_num, function(x) dir.create(file.path(maindir, "output", subdir, paste("s", x, sep=""))))

  ctlf <- file.path(maindir, "em_input", "amak.dat")
  datf <- file.path(maindir, "em_input", "amak_data.dat")

  modify_input = "partial"
  for (om_sim in 1:om_sim_num){
    load(file=file.path(maindir, "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    if(modify_input == "all") {

    }

    if(modify_input == "partial") {
      char.lines <- readLines(ctlf)
      char.lines[grep("#SigmaR", char.lines)+1] <- gsub(gsub(" .*$", "", char.lines[grep("#SigmaR", char.lines)+1]), om_input$logR_sd, char.lines[grep("#SigmaR", char.lines)+1]) #Replace the value before 1st space
      char.lines[grep("#catchability", char.lines)+1] <- gsub(gsub(" .*$", "", char.lines[grep("#catchability", char.lines)+1]), em_input$survey_q$survey1, char.lines[grep("#catchability", char.lines)+1])
      writeLines(char.lines, con=file.path(maindir, "output", subdir, paste("s", om_sim, sep=""), "amak.dat"))

      char.lines <- readLines(datf)
      char.lines[grep("#styr\t", char.lines)+1] <- as.character(paste((em_input$catch_data_year[1]), collapse="\t"))
      char.lines[grep("#endyr\t", char.lines)+1] <- as.character(paste((em_input$catch_data_year[length(em_input$catch_data_year)]), collapse="\t"))
      char.lines[grep("#catch\t", char.lines)+1] <- as.character(paste(em_input$L.obs$fleet1, collapse="\t"))
      char.lines[grep("#catch_cv", char.lines)+1] <- as.character(paste(rep(em_input$cv.L$fleet1, length(em_input$L.obs$fleet1)), collapse="\t"))
      char.lines[grep("#n_ages_fsh", char.lines)+1] <- as.character(paste(length(em_input$catch_age_data_year), collapse="\t"))
      char.lines[grep("#yrs_ages_fsh", char.lines)+1] <- as.character(paste(em_input$catch_age_data_year, collapse="\t"))
      char.lines[grep("#sample_ages_fsh", char.lines)+1] <- as.character(paste(rep(em_input$n.L$fleet1, length(em_input$catch_age_data_year)), collapse="\t"))

      for (i in 1:nrow(em_input$L.age.obs$fleet1)){
        char.lines[grep("#page_fsh", char.lines)+i]<-as.character(paste(em_input$L.age.obs$fleet1[i,],collapse="\t"))
      }

      for (i in 1:length(em_input$L.obs$fleet1)){
        char.lines[grep("#wt_age_fsh", char.lines)+i]<-as.character(paste(om_input$W.mt,collapse="\t"))
      }



      char.lines[grep("#nobs_ind", char.lines)+1] <- as.character(paste(length(em_input$survey_index_year), collapse="\t"))
      char.lines[grep("#yrs_ind", char.lines)+1] <- as.character(paste(em_input$survey_index_year, collapse="\t"))
      char.lines[grep("#biom_ind\t", char.lines)+1] <- as.character(paste(em_input$survey.obs$survey1, collapse="\t"))
      char.lines[grep("#biom_cv", char.lines)+1] <- as.character(paste(em_input$cv.survey$survey1*em_input$survey.obs$survey1, collapse="\t"))
      char.lines[grep("#n_ages_ind", char.lines)+1] <- as.character(paste(length(em_input$survey_age_data_year), collapse="\t"))
      char.lines[grep("#yrs_ages_ind", char.lines)+1] <- as.character(paste(em_input$survey_age_data_year, collapse="\t"))
      char.lines[grep("#sample_ages_ind", char.lines)+1] <- as.character(paste(rep(em_input$n.survey$survey1, length(em_input$survey.obs$survey1)), collapse="\t"))


      #char.lines[grep("#biom_cv", char.lines)+1] <- as.character(paste(em_input$cv.survey$survey1*em_input$survey.obs$survey1/em_input$survey.obs$survey1, collapse="\t"))
      for (i in 1:nrow(em_input$survey.age.obs$survey1)){
        char.lines[grep("#page_ind", char.lines)+i]<-as.character(paste(em_input$survey.age.obs$survey1[i,],collapse="\t"))
      }

      for (i in 1:length(om_input$yrs)){
        char.lines[grep("#wt_age_ind", char.lines)+i]<-as.character(paste(om_input$W.mt/om_input$W.mt,collapse="\t"))
      }


      writeLines(char.lines, con=file.path(maindir, "output", subdir, paste("s", om_sim, sep=""),  "amak_data.dat"))
    }

    file.copy(file.path(maindir, "em_input", "amak.exe"), file.path(maindir, "output", subdir, paste("s", om_sim, sep=""), "amak.exe"), overwrite = T)
  }

  for (om_sim in 1:om_sim_num){
    setwd(file.path(maindir, "output", subdir, paste("s", om_sim, sep="")))
    system(paste("amak.exe amak.dat", sep = ""), show.output.on.console = FALSE)
  }
}