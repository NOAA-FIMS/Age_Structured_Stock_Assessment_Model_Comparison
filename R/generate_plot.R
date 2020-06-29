generate_plot <- function(em_names=NULL, plot_ncol=NULL, plot_nrow=NULL, plot_color=NULL){
  col = c("black", plot_color)

  #### Check convergence and plot gradient histogram ####
  check_convergence(em_names, om_sim_num, col=col, plot_ncol, plot_nrow)

  #### Check plotted individual iteration real ID ####
  if(om_sim_num <= figure_number) {
    figure_id <- 1:om_sim_num
  } else{
    figure_id <- seq(1, om_sim_num, by=round((om_sim_num-1)/(figure_number-1)))
  }
  real_figure_id <- keep_sim_id[figure_id]
  write.csv(real_figure_id, file=file.path(maindir, "output", "Real_Figure_ID.csv"))

  read_plot_data(em_names)

}
