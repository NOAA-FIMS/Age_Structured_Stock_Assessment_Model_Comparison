#### MSY based estimates plot ####
maindir_list  <- c("C:/Users/bai.li/Desktop/mcp_results_r/cases/case2_1", 
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case2_2", 
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case2_3", 
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case3_1", 
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case4_1", 
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case4_2_1",
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case4_2_2",
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case4_2_3",
                   "C:/Users/bai.li/Desktop/mcp_results_r/cases/case5_1_doublelogistic_fleetsurvey") 

em_num <- 4

for(j in 1:length(maindir_list)){
  load(file.path(maindir_list[j], "output", "performance_measures_output.RData"))
  if (j==1){
    msy_re <- matrix(NA, nrow=length(pm_list), ncol=em_num*length(maindir_list))
    fmsy_re <- matrix(NA, nrow=length(pm_list), ncol=em_num*length(maindir_list))
    ssbmsy_re <- matrix(NA, nrow=length(pm_list), ncol=em_num*length(maindir_list))
  }
  for (i in 1:length(pm_list)){
    msy_re[i,((j-1)*4+1):((j-1)*4+4)] <- as.matrix(pm_list[[i]]$msy$re)
    fmsy_re[i,((j-1)*4+1):((j-1)*4+4)] <- as.matrix(pm_list[[i]]$fmsy$re)
    ssbmsy_re[i,((j-1)*4+1):((j-1)*4+4)] <- as.matrix(pm_list[[i]]$ssbmsy$re)
  }
}

msy_stat <- matrix(NA, nrow=3, ncol=em_num*length(maindir_list))
fmsy_stat <- matrix(NA, nrow=3, ncol=em_num*length(maindir_list))
ssbmsy_stat <- matrix(NA, nrow=3, ncol=em_num*length(maindir_list))

sapply(1:ncol(msy_stat), function(x) {
  msy_stat[,x] <<- boxplot.stats(msy_re[,x])$`stats`[c(1,3,5)]
  fmsy_stat[,x] <<- boxplot.stats(fmsy_re[,x])$`stats`[c(1,3,5)]
  ssbmsy_stat[,x] <<- boxplot.stats(ssbmsy_re[,x])$`stats`[c(1,3,5)]
})

jpeg(file="C:/Users/bai.li/Desktop/mcp_results_r/cases/manuscript_figures/MSY_RE.jpg", width=150, height=150, units="mm", res=300)
op<-par(no.readonly=TRUE)
par(op)
par(oma=c(2,2,0,4),mar=c(4,0,0,0.5),mfrow=c(1,3),pch=16)
color=c("orange", "green", "red", "deepskyblue3")
xlim=c(-0.5, 0.5)

plot(msy_stat[2,], rev(1:ncol(msy_stat)), 
     pch=rep(((1:em_num)+1), times=length(maindir_list)),
     col=color, 
     xlim=xlim, 
     xlab="RE in MSY", ylab="", 
     axes=F)
segments(msy_stat[1,], rev(1:ncol(msy_stat)), msy_stat[3,], rev(1:ncol(msy_stat)), col=rep(color, times=length(maindir_list)))
abline(v=0, col="gray", lty=2)
abline(h=seq(em_num+0.5, ncol(msy_stat), by=em_num), col="gray30", lty=2)
box()
axis(1)
text(x=rep(min(xlim)*0.9, times=length(maindir_list)), 
     y=seq(em_num, ncol(msy_stat), by=4), 
     rev(paste("C", 0:(length(maindir_list)-1), sep="")))

plot(fmsy_stat[2,], rev(1:ncol(fmsy_stat)), 
     pch=rep(((1:em_num)+1), times=length(maindir_list)),
     col=color, 
     xlim=xlim, 
     xlab="RE in FMSY", ylab="", 
     axes=F)
segments(fmsy_stat[1,], rev(1:ncol(fmsy_stat)), fmsy_stat[3,], rev(1:ncol(fmsy_stat)), col=rep(color, times=length(maindir_list)))
abline(v=0, col="gray", lty=2)
abline(h=seq(em_num+0.5, ncol(fmsy_stat), by=em_num), col="gray30", lty=2)
box()
axis(1)
text(x=rep(min(xlim)*0.9, times=length(maindir_list)), 
     y=seq(em_num, ncol(fmsy_stat), by=4), 
     rev(paste("C", 0:(length(maindir_list)-1), sep="")))

plot(ssbmsy_stat[2,], rev(1:ncol(ssbmsy_stat)), 
     pch=rep(((1:em_num)+1), times=length(maindir_list)),
     col=color, 
     xlim=xlim, 
     xlab="RE in SSBMSY", ylab="", 
     axes=F)
segments(ssbmsy_stat[1,], rev(1:ncol(ssbmsy_stat)), ssbmsy_stat[3,], rev(1:ncol(ssbmsy_stat)), col=rep(color, times=length(maindir_list)))
abline(v=0, col="gray", lty=2)
abline(h=seq(em_num+0.5, ncol(ssbmsy_stat), by=em_num), col="gray30", lty=2)
box()
axis(1)
text(x=rep(min(xlim)*0.9, times=length(maindir_list)), 
     y=seq(em_num, ncol(ssbmsy_stat), by=4), 
     rev(paste("C", 0:(length(maindir_list)-1), sep="")))

legend(x=max(xlim)*1.05,y=ncol(msy_stat)/2*1.2,
       legend=c("AMAK", "ASAP", "BAM", "SS"),
       pch=c(2, 3, 4, 5),
       lty=c(1, 1, 1, 1), 
       col=c("orange", "green", "red", "deepskyblue3"), 
       cex=1, 
       bty="n", 
       xpd=NA)
dev.off()