i=4

 jpeg(file=file.path(maindir, subdir, paste(title[i], "_re_sim_pressntation.jpg", sep="")), width=150, height=100, units="mm", res=300)
  op<-par(no.readonly=TRUE)
  par(op)
  par(oma=c(2,2,0,4),mar=c(3,3,0.5,0),mfrow=c(2,2),pch=16)
  #par(mar=c(0.7, 4, 0.2, 0.2), mfrow=c(2,2), oma = c(4, 4, 0.2, 0.2))
  for(k in 1:4){
    temp <- matrix(NA, ncol=om_sim_num, nrow=om_input$nyr)
    re_quantile <- matrix(NA, ncol=om_input$nyr, nrow=5)
    re_quantile_list = list()

    for(j in 1:om_sim_num){
      temp[,j] <- pm_list[[j]][[plot_data_id[i]]]$re[,k]
    }
    re_quantile <- sapply(1:om_input$nyr, function(x) quantile(temp[x,], c(0.1, 0.25, 0.5, 0.75, 0.9)))
    plot(re_quantile[3,], type="l", col="white", ylim=c(-max(abs(re_quantile), 0.3), max(abs(re_quantile), 0.3)), xlab="", ylab="", panel.first=grid(lty=1))
    polygon(c(om_output$yr, rev(om_output$yr)), c(re_quantile[1,], rev(re_quantile[5,])), border=NA, col=col[k])
    polygon(c(om_output$yr, rev(om_output$yr)), c(re_quantile[2,], rev(re_quantile[4,])), border=NA, col=light_col[k])
    lines(re_quantile[3,], type="l", col="white")

    box()

    legend("topleft", legend=paste(legend[k], ":", title[i], sep=""), bty="n", text.font = 2)
    mtext(text="Year",side=1,line=0,outer=TRUE, font=2)
    mtext(text="Relative error",side=2,line=0,outer=TRUE, font=2)

  }

  dev.off()