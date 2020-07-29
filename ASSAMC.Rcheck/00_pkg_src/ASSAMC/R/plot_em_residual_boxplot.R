plot_em_residual_boxplot <- function(em_names, col){
  for(i in 1:length(em_names)){
    jpeg(file=file.path(maindir, "figure", paste(em_names[i], "_residual_boxplot.jpg"), sep=""), width=170, height=160, units="mm", res=300)

    mat=matrix(1:9, ncol=3, nrow=3, byrow = T)
    layout(mat=mat, widths=rep.int(1, ncol(mat)), heights=rep.int(1, nrow(mat)))
    par(las=1, mar=c(4.5,4.5,1,0.5))

    comparison_var <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "fratio", "ssbratio")
    comparison_id <- which(names(om_list) %in% comparison_var)
    xlab <- rep("Year", times=length(comparison_var))
    ylab <- rep("Residuals", times=length(comparison_var))
    legend <- c("Biomass (mt)", "Abundance (1000 fish)", "SSB (mt)", "Recruitment (1000 fish)", "F", "Landings (mt)", "Survey Index (scaled)", "F/FMSY", "SSB/SSBMSY")

    for(j in 1:length(comparison_id)){
      xlim <- range(year)
      residual_data <- (em_list[[i]][[comparison_id[j]]] - om_list[[comparison_id[j]]]) / om_list[[comparison_id[j]]]

      boxplot(t(residual_data), xlab=xlab[j], ylab=ylab[j], col=col[i+1], notch=F, staplelwd = 1, pch=19, cex=0.5)
      abline(h=0, col="coral3")
      legend("topleft", legend[j], bty="n")
    }
    dev.off()
  }
}
