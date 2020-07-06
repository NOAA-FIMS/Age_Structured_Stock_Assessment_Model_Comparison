plot_em_re <- function(em_names, col, plot_nrow, plot_ncol){
  comparison_var <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "fratio", "ssbratio")
  comparison_id <- which(names(om_list) %in% comparison_var)
  title <- c("Biomass", "Abundance", "SSB", "Recruitment", "F", "Landings", "Survey Index", "Fratio", "SSBratio")
  light_col <- rgb(t(col2rgb(col)/1.4), maxColorValue = 357)

  for (i in 1:length(comparison_id)){
    jpeg(file=file.path(maindir, "figure", paste(title[i], "_re_comparison.jpg", sep="")), width=200, height=120, units="mm", res=300)
    par(mar=c(4, 4, 0.2, 0.2), mfrow=c(plot_nrow, plot_ncol), oma = c(4, 4, 0.2, 0.2))
    for(k in 1:length(em_names)){
      temp <- matrix(NA, ncol=keep_sim_num, nrow=length(year))
      re_quantile <- matrix(NA, ncol=length(year), nrow=5)
      re_quantile_list <- list()

      for(j in 1:keep_sim_num){
        temp[,j] <- re_list[[j]][[comparison_id[i]]][,k]
      }

      re_quantile <- sapply(1:length(year), function(x) quantile(temp[x,], c(0.1, 0.25, 0.5, 0.75, 0.9)))
      plot(re_quantile[3,],
           type="l",
           col="white",
           ylim=c(-max(abs(re_quantile), 0.3), max(abs(re_quantile), 0.3)),
           xlab="Year",
           ylab="Relative Error",
           panel.first=grid(lty=1))
      polygon(c(year, rev(year)), c(re_quantile[1,], rev(re_quantile[5,])), border=NA, col=col[k+1])
      polygon(c(year, rev(year)), c(re_quantile[2,], rev(re_quantile[4,])), border=NA, col=light_col[k+1])
      lines(re_quantile[3,], type="l", col="white")
      box()
      legend("topleft", legend=paste(em_names[k], ":", title[i], sep=""), bty="n")
    }
    dev.off()
  }
}
