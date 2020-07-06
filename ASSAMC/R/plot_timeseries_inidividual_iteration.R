plot_timeseries_inidividual_iteration <- function(em_names, col){
  comparison_var <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey")
  comparison_id <- which(names(om_list) %in% comparison_var)
  ylab <- c("Biomass (mt)", "Abundance (1000 fish)", "SSB (mt)", "Recruitment (1000 fish)", "F", "Landings (mt)", "Survey Index (scaled)")
  xlab = rep("Year", times=length(ylab))

  nrow <- 2
  ncol <- 4

  for (j in 1:length(real_figure_id)){
    jpeg(file=file.path(maindir, "figure", paste("Fig", real_figure_id[j], "_timeseries_inidividual_iteration.jpg", sep="")), width=205, height=75, units="mm", res=300)

    par(mar=c(0.7, 4, 0.2, 0.2), mfrow=c(nrow,ncol), oma = c(4, 4, 0.2, 0.2))

    for(i in 1:length(comparison_id)){
      ylim=c(min(om_list[[comparison_id[i]]])*0.5, max(om_list[[comparison_id[i]]])*1.5)

      plot(year, om_list[[comparison_id[i]]][,j], pch=19, col=col[1], cex=0.7, ylim=ylim, axes=F, xlab="", ylab="")
      invisible(sapply(1:length(em_names), function(x) lines(year, em_list[[x]][[comparison_id[i]]][,j], type="l", col=col[x+1], lwd=1.5)))

      if(i < ncol) axis(1, labels = F)
      else {
        axis(1)
        mtext(side=1, text=xlab, line=2, cex=0.6, font=2, col="blue")
      }
      mtext(side=2, text=ylab[i], line=3.1, cex=0.6, font=2, col="blue")
      axis(2, las=2)
      box()
    }
    plot.new()
    legend("bottom", c("OM", em_names), pch=c(19, rep(NA, length=length(em_names))), lty=c(NA,rep(1, length=length(em_names))), lwd=c(NA, rep(1.5, length=length(em_names))), col=col, bty="n", cex=0.8)
    dev.off()
  }
}

