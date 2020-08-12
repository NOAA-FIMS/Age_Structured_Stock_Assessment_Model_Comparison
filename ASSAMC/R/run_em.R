run_em <- function(em_names=NULL, input_list=NULL){

  if (!file.exists(file.path(maindir, "em_input"))) stop ("Missing estimation model input file!")
  if (is.null(em_names)) stop ("Missing EM information!")

  maindir <- input_list$maindir
  om_sim_num <- input_list$om_sim_num
  case_name <- input_list$case_name
  casedir <- file.path(maindir, case_name)
  em_bias_cor <- input_list$em_bias_cor

  invisible(sapply(em_names, function(x) {
    if (!file.exists(file.path(casedir, "output", x))) dir.create(file.path(casedir, "output", x))
  }))

  if("AMAK" %in% em_names) run_amak(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir, )
  if("ASAP" %in% em_names) run_asap(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir)
  if("BAM" %in% em_names) run_bam(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir, em_bias_cor=em_bias_cor)
  if("SS" %in% em_names) run_ss(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir, em_bias_cor=em_bias_cor)
  if("MAS" %in% em_names) run_mas(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir)
}
