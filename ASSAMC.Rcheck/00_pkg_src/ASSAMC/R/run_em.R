run_em <- function(run_em_names=NULL){
  if (is.null(run_em_names)) stop ("Missing EM information!")

  invisible(sapply(run_em_names, function(x) {
    if (!file.exists(file.path(maindir, "output", x))) dir.create(file.path(maindir, "output", x))
  }))

  if("AMAK" %in% run_em_names) run_amak(maindir=maindir, om_sim_num=om_sim_num)
  if("ASAP" %in% run_em_names) run_asap(maindir=maindir, om_sim_num=om_sim_num)
  if("BAM" %in% run_em_names) run_bam(maindir=maindir, om_sim_num=om_sim_num)
  if("SS" %in% run_em_names) run_ss(maindir=maindir, om_sim_num=om_sim_num)
  if("MAS" %in% run_em_names) run_mas(maindir=maindir, om_sim_num=om_sim_num)
}
