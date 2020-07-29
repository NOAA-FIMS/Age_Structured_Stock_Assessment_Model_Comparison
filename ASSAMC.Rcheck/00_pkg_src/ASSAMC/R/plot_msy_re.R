plot_msy_re <- function(em_names, col){
  msy_re <- matrix(NA, ncol=length(em_names)*3, nrow=keep_sim_num)
  for(i in 1:keep_sim_num){
    msy_re[i,]<-c(as.numeric(as.character(re_list[[i]]$msy)), as.numeric(as.character(re_list[[i]]$fmsy)), as.numeric(as.character(re_list[[i]]$ssbmsy)))
  }

  jpeg(file=file.path(maindir, "figure", "msy_re_sim.jpg"), width=150, height=120, units="mm", res=300)
  par(mar = c(4, 8, 4, 2) + 0.1, mfrow=c(1,1))
  labels <- c(paste(em_names, "_MSY", sep=""),
              paste(em_names, "_FMSY", sep=""),
              paste(em_names, "_SSBMSY", sep=""))

  msy_sim_boxplot <- boxplot(msy_re[,seq(dim(msy_re)[2],1)],
                             horizontal=T,
                             col=col[length(col):2],
                             pch=16, cex=0.5, axes=F,
                             at=c(1:length(em_names),  (length(em_names)+2):(length(em_names)*2+1), (length(em_names)*2+3):(length(em_names)*3+2)),
                             xlab="Relative Error",
                             ylim=c(min(-0.5, min(msy_re)), max(0.5, max(msy_re))))



  abline(v=0, col="coral3", lty=2)
  box()
  axis(1)
  axis(2,
       at=c(1:length(em_names),  (length(em_names)+2):(length(em_names)*2+1), (length(em_names)*2+3):(length(em_names)*3+2)),
       labels = rev(labels),
       las=2)
  dev.off()
}
