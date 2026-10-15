#' @export
run_asap <- function(maindir=NULL, subdir="ASAP", om_sim_num=NULL, casedir=casedir, input_filename=NULL){
  if(!("ASAPplots" %in% installed.packages()[,"Package"])) devtools::install_github("cmlegault/ASAPplots", build_vignettes = TRUE)
  library(ASAPplots)

  setwd(file.path(casedir, "output", subdir))

  file.copy(file.path(maindir, "em_input", paste("asap3_", input_filename, ".DAT", sep="")), file.path(maindir, "em_input", "asap3.DAT"), overwrite = T)

  unlink(list.files(file.path(casedir, "output", "ASAP"), full.names = TRUE), recursive = TRUE)

  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep=""))))

  asap_input <- ReadASAP3DatFile(file.path(maindir, "em_input", "asap3.DAT"))
  modify_input = "partial"

  invisible(lapply(1:om_sim_num, function(om_sim) {
    load(file=file.path(casedir, "output","OM", paste("OM", om_sim, ".RData", sep="")))
    if(modify_input == "all") {
      asap_input$dat$n_years <- par.sim1$nyr
      asap_input$dat$year1 <- sim1$yr[1]
      asap_input$dat$n_ages <- length(par.sim1$ages)
      asap_input$dat$n_fleets <- 1 #needs modification in om
      asap_input$dat$n_fleet_sel_blocks <- 1 #needs modification in om
      asap_input$dat$n_indices <- 1 #needs modification in om
      asap_input$dat$M <- matrix(rep(par.sim1$M.age, times=par.sim1$nyr), nrow=par.sim1$nyr, byrow = T)
      asap_input$dat$fec_opt <- 0 #needs modifcation in asap file
      asap_input$dat$fracyr_spawn <- 0 #needs modifcation in om/asap file (month-1)/12
      asap_input$dat$maturity <- matrix(rep(par.sim1$mat.age*par.sim1$proportion.female, times=par.sim1$nyr), nrow=par.sim1$nyr, byrow = T)
      asap_input$dat$n_WAA_mats <- 1 #needs modification in asap file
      asap_input$dat$WAA_mats[[1]] <- matrix(rep(par.sim1$W.kg, times=par.sim1$nyr), nrow=par.sim1$nyr, byrow = T) #needs modification in asap file (list index)
      asap_input$dat$WAA_pointers <- matrix(c(1,1,1,1,1,1), nrow=6, byrow = T) #needs modification in asap file
    } #incomplement
    if(modify_input == "partial") {
      asap_input$dat$CAA_mats[[1]] <- cbind(em_input$L.age.obs$fleet1, em_input$L.obs$fleet1)
      asap_input$dat$catch_cv <- cbind(rep(em_input$cv.L$fleet1, times=length(em_input$L.obs$fleet1)))
      for (survey_id in 1:om_input$survey_num){
        asap_input$dat$IAA_mats[[survey_id]] <- cbind(om_input$year, em_input$survey.obs[[survey_id]], rep(em_input$cv.survey[[survey_id]], times=length(em_input$survey.obs[[survey_id]])), em_input$survey.age.obs[[survey_id]], rep(em_input$n.survey[[survey_id]], times=length(em_input$survey.obs[[survey_id]])))
      }

      asap_input$dat$catch_Neff <- cbind(rep(em_input$n.L$fleet1, length(em_input$L.obs$fleet1)))
      asap_input$dat$F1_ini <- om_output$f[1]
      asap_input$dat$q_ini <- unlist(em_input$survey_q)*1000
      asap_input$dat$N1_ini <- om_input$N.pr0*1000
      asap_input$dat$recruit_cv <- cbind(rep(sqrt(exp(om_input$logR_sd^2)-1), om_input$nyr))
    }
    WriteASAP3DatFile(fname = file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "asap3.DAT"), dat.object=asap_input, header.text = "")
  }))

  cl <- ifelse(detectCores()==1, detectCores(), detectCores()-2)
  registerDoParallel(cl)

  foreach (om_sim = 1:om_sim_num) %dopar% {
    setwd(file.path(casedir, "output", subdir, paste("s", om_sim, sep="")))
    file.copy(file.path(maindir, "em_input", "ASAP3.exe"), file.path(casedir,"output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"), overwrite = T)
    # system(paste(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"), file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "asap3.DAT"), sep = " "), show.output.on.console = FALSE)
    # Define the path to the executable and its data file
    exe_path <- file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe")
    dat_path <- file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "asap3.DAT")
    is_win   <- (.Platform[["OS.type"]] == "windows")

    # Cross-platform execution helper
    exec_asap <- function(args) {
      if (is_win) {
        system2(exe_path, args = args, stdout = FALSE, stderr = FALSE)
      } else {
        system(paste("chmod +x", shQuote(exe_path)))
        system2("wine", args = c(shQuote(exe_path), args), stdout = FALSE, stderr = FALSE)
      }
    }

    # Run optimization + reporting only (-nohess)
    t_opt_rep <- system.time({
      exec_asap(c(dat_path, "-nohess"))
    })[["elapsed"]]

    # Run reporting only (0 iterations from the converged .par file)
    t_rep_only <- system.time({
      exec_asap(c(dat_path, "-maxfn 0 -nohess -ainp asap3.par"))
    })[["elapsed"]]

    # Full evaluation (Optimization + Hessian + Reporting)
    t_full <- system.time({
      exec_asap(dat_path)
    })[["elapsed"]]

    # Isolate components
    t_optimization <- max(0, t_opt_rep - t_rep_only)
    t_sdreport     <- max(0, t_full - t_opt_rep)
    t_fit_total    <- t_optimization + t_sdreport

    # Save timing tibble conforming to your cross-platform schema
    run_time_asap <- tibble::tibble(
      component = c(
        "fit_optimization",
        "fit_sdreport",
        "fit_total",
        "total",
        "tmb_sdreport"
      ),
      time = c(
        as.numeric(t_optimization),
        as.numeric(t_sdreport),
        as.numeric(t_fit_total),
        as.numeric(t_full),
        NA_real_
      ),
      unit = "seconds"
    )

    saveRDS(
      run_time_asap,
      file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "runtime_asap.RDS")
    )

    
    file.remove(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"))
    file_list <- list.files(path = getwd())
    file.remove(c(file_list[!(file_list %in% c(list.files(path = getwd(), pattern = c(".rep")),
                                               list.files(path = getwd(), pattern = c(".par")),
                                               list.files(path = getwd(), pattern = c(".std")),
                                               list.files(path = getwd(), pattern = c(".rdat")),
                                               list.files(path = getwd(), pattern = c(".cov")),
                                               list.files(path = getwd(), pattern = c(".DAT")),
                                               list.files(path = getwd(), pattern = c(".RDS"))))]))
  }
  # stopCluster(cl)

  # invisible(lapply(1:om_sim_num, function(om_sim) {
  #
  #   setwd(file.path(casedir, "output", subdir, paste("s", om_sim, sep="")))
  #   file.copy(file.path(maindir, "em_input", "ASAP3.exe"), file.path(casedir,"output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"), overwrite = T)
  #   system(paste(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"), file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "asap3.DAT"), sep = " "), show.output.on.console = FALSE)
  #   file.remove(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "ASAP3.exe"))
  # }))
}
