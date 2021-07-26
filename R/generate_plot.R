#' @export
generate_plot <- function(em_names=NULL,
                          plot_ncol=NULL, plot_nrow=NULL, plot_color=NULL,
                          input_list=NULL,
                          adhoc_bias_cor=FALSE){
  if (is.null(em_names)) stop ("Missing EM information!")
  if (plot_ncol*plot_nrow < length(em_names)) stop ("Add more columns or rows to plot all EM outputs!")
  if (length(plot_color) != length(em_names)) stop ("Number of colors doesn't equal to number of EMs")
  if (is.null(input_list)) stop ("Missing input_list!")

  maindir <- input_list$maindir
  case_name <- input_list$case_name
  om_sim_num <- input_list$om_sim_num
  casedir <- file.path(maindir, case_name)

  invisible(sapply(c("figure"), function(x) {
    if (!file.exists(file.path(casedir, x))) dir.create(file.path(casedir, x))
  }))
  unlink(list.files(file.path(casedir, "figure"), full.names = TRUE), recursive = TRUE)

  col <- c("black", plot_color)

  keep_sim_num <- input_list$keep_sim_num
  figure_number <- input_list$figure_number

  #### Check convergence and plot gradient histogram ####
  keep_sim_id <- check_convergence(em_names, om_sim_num, col=col, plot_ncol, plot_nrow, casedir=casedir)

  if(length(na.omit(keep_sim_id)) < keep_sim_num) cat("Increase om_sim_num to get enough number of converged iterations!")
  keep_sim_id <- na.omit(keep_sim_id)
  keep_sim_num <- length(keep_sim_id)
  input_list$keep_sim_num <- length(keep_sim_id)

  #### Check plotted individual iteration real ID ####
  if(keep_sim_num <= figure_number) {
    figure_id <- 1:keep_sim_num
  } else{
    figure_id <- seq(1, keep_sim_num, by=round((keep_sim_num-1)/(figure_number-1)))
  }
  real_figure_id <<- keep_sim_id[figure_id]
  write.csv(real_figure_id, file=file.path(casedir, "output", "Real_Figure_ID.csv"))

  read_plot_data(em_names=em_names, casedir=casedir, keep_sim_num=keep_sim_num, adhoc_bias_cor=adhoc_bias_cor, SRmodel=input_list$SRmodel)

  check_performance(em_names, casedir=casedir)

  plot_timeseries_inidividual_iteration(em_names=em_names, col=col, casedir=casedir, input_list=input_list)

  plot_ratio_inidividual_iteration(em_names=em_names, col=col, casedir=casedir, input_list=input_list)

  plot_agecomp_individual_iteration(em_names=em_names, col=col, casedir=casedir)

  plot_em_residual_boxplot(em_names=em_names, col=col, casedir=casedir)

  plot_stock_status_determination(em_names=em_names, col=col, casedir=casedir)

  plot_msy_re(em_names=em_names, col=col, casedir=casedir)

  plot_em_re(em_names=em_names, col=col, plot_nrow=plot_nrow, plot_ncol=plot_ncol, casedir=casedir, input_list=input_list)

  plot_timeseries_all_iterations(em_names=em_names, col=col, plot_nrow=plot_nrow, plot_ncol=plot_ncol, casedir=casedir)

}
