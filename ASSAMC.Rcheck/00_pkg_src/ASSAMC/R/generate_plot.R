generate_plot <- function(em_names=NULL, plot_ncol=NULL, plot_nrow=NULL, plot_color=NULL){

  invisible(sapply(c("figure"), function(x) {
    if (!file.exists(file.path(maindir, x))) dir.create(file.path(maindir, x))
  }))

  col <- c("black", plot_color)

  #### Check convergence and plot gradient histogram ####
  check_convergence(em_names, om_sim_num, col=col, plot_ncol, plot_nrow)

  #### Check plotted individual iteration real ID ####
  if(keep_sim_num <= figure_number) {
    figure_id <- 1:keep_sim_num
  } else{
    figure_id <- seq(1, keep_sim_num, by=round((keep_sim_num-1)/(figure_number-1)))
  }
  real_figure_id <<- keep_sim_id[figure_id]
  write.csv(real_figure_id, file=file.path(maindir, "output", "Real_Figure_ID.csv"))

  read_plot_data(em_names)

  check_performance(em_names)

  plot_timeseries_inidividual_iteration(em_names=em_names, col=col)

  plot_ratio_inidividual_iteration(em_names=em_names, col=col)

  plot_agecomp_individual_iteration(em_names=em_names, col=col)

  plot_em_residual_boxplot(em_names=em_names, col=col)

  plot_stock_status_determination(em_names=em_names, col=col)

  plot_msy_re(em_names=em_names, col=col)

  plot_em_re(em_names=em_names, col=col, plot_nrow=plot_nrow, plot_ncol=plot_ncol)

  plot_timeseries_all_iterations(em_names=em_names, col=col, plot_nrow=plot_nrow, plot_ncol=plot_ncol)

}
