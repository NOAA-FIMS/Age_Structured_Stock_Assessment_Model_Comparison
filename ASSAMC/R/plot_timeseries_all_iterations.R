plot_timeseries_all_iterations <- function(em_names, col, plot_nrow, plot_ncol){
  comparison_var <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "fratio", "ssbratio")
  comparison_id <- which(names(om_list) %in% comparison_var)
  title = c("Biomass", "Abundance", "SSB", "Recruitment", "F", "Landings", "Survey Index", "Fratio", "SSBratio")
  light_col <- rgb(t(col2rgb(col)/1.4), maxColorValue = 357)

  for (i in 1:length(comparison_id)){
    jpeg(file=file.path(maindir, "figure", paste(title[i], "_all_iterations.jpg", sep="")), width=200, height=120, units="mm", res=300)
    par(mar=c(4, 4, 0.2, 0.2), mfrow=c(plot_nrow, plot_ncol), oma = c(4, 4, 0.2, 0.2))

    ylim=c(min(om_list[[comparison_id[i]]])*0.5, max(om_list[[comparison_id[i]]])*1.5)
    for(k in 1:length(em_names)){
      plot(om_list[[comparison_id[i]]][,1], type="l", col="gray50", xlab="Year", ylab=title[i], panel.first=grid(lty=1), ylim=ylim)
      sapply(1:keep_sim_num, function(x) lines(em_list[[k]][[comparison_id[i]]][,x], col=col[k+1], lty=2))
      sapply(1:keep_sim_num, function(x) lines(om_list[[comparison_id[i]]][,x], col="gray50", type="l", pch=19, cex=0.5))
      box()
      legend("topleft",
             legend=c("OM", em_names[k]),
             lty=c(1, 2),
             col=c("gray50", col[k+1]),
             bty="n")
    }
    dev.off()
  }
}
