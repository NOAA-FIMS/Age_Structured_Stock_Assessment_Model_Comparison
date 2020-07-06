plot_agecomp_individual_iteration <- function(em_names, col){
  comparison_var <- c("agecomp")
  comparison_id <- which(names(om_list) %in% comparison_var)

  ylab <- c("Proportion")
  xlab = c("Age")

  nrow <- 1
  ncol <- 3

  for(j in 1:length(real_figure_id)){
    jpeg(file=file.path(maindir, "figure", paste("Fig", j, "_agecomp_last.jpg", sep="")), width=205, height=75, units="mm", res=300)

    par(mar=c(0.7, 4, 0.2, 0.2), mfrow=c(nrow, ncol), oma = c(4, 4, 0.2, 0.2))

    for(i in 1:length(comparison_id)){
      ylim=c(min(om_list[[comparison_id[i]]][[j]][,1])*0.5, max(om_list[[comparison_id[i]]][[j]][,1])*1.5)

      plot(ages, om_list[[comparison_id[i]]][[j]][,1], pch=19, col=col[1], cex=0.7, ylim=ylim, axes=F, xlab="", ylab="")
      invisible(sapply(1:length(em_names), function(x) lines(ages, em_list[[x]][[comparison_id[i]]][[j]][,1], type="l", col=col[x+1], lwd=1.5)))

      axis(1)
      mtext(side=1, text=xlab, line=2, cex=0.6, font=2, col="blue")
      mtext(side=2, text=ylab[i], line=3.1, cex=0.6, font=2, col="blue")
      axis(2, las=2)
      box()
      legend("topright", paste("Year ", year[1], sep=""), bty="n")

      ylim=c(min(om_list[[comparison_id[i]]][[j]][,length(year)])*0.5, max(om_list[[comparison_id[i]]][[j]][,length(year)])*1.5)

      plot(ages, om_list[[comparison_id[i]]][[j]][,length(year)], pch=19, col=col[1], cex=0.7, ylim=ylim, axes=F, xlab="", ylab="")
      invisible(sapply(1:length(em_names), function(x) lines(ages, em_list[[x]][[comparison_id[i]]][[j]][,length(year)], type="l", col=col[x+1], lwd=1.5)))

      axis(1)
      mtext(side=1, text=xlab, line=2, cex=0.6, font=2, col="blue")
      mtext(side=2, text=ylab[i], line=3.1, cex=0.6, font=2, col="blue")
      axis(2, las=2)
      box()
      legend("topright", paste("Year ", year[length(year)], sep=""), bty="n")
    }
    plot.new()
    legend("topleft", c("OM", em_names), pch=c(19, rep(NA, length=length(em_names))), lty=c(NA,rep(1, length=length(em_names))), lwd=c(NA, rep(1.5, length=length(em_names))), col=col, bty="n", cex=0.8)
    dev.off()
  }
}
