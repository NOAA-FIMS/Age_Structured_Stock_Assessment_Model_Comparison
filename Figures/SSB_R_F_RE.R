#### SSB, R, and F ####
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

ssb_median <- ssb_low <- ssb_high <- r_median <- r_low <- r_high <- f_median <- f_low <- f_high <- matrix(NA, nrow=nrow(pm_list[[1]]$ssb$re), ncol=em_num*length(maindir_list))

for(j in 1:length(maindir_list)){
  load(file.path(maindir_list[j], "output", "performance_measures_output.RData"))
  if (j==1){
    ssb_re <- matrix(NA, nrow=length(pm_list), ncol=nrow(pm_list[[1]]$ssb$re))
    r_re <- matrix(NA, nrow=length(pm_list), ncol=nrow(pm_list[[1]]$recruit$re))
    f_re <- matrix(NA, nrow=length(pm_list), ncol=nrow(pm_list[[1]]$Ftot$re))
  }
  for (k in 1:em_num){
    for (i in 1:length(pm_list)){
      ssb_re[i,] <- as.matrix(pm_list[[i]]$ssb$re[,k])
      r_re[i,] <- as.matrix(pm_list[[i]]$recruit$re[,k])
      f_re[i,] <- as.matrix(pm_list[[i]]$Ftot$re[,k])
    }
    ssb_median[,((j-1)*4+k)] <- sapply(1:ncol(ssb_re), function(x) boxplot.stats(ssb_re[,x])$`stats`[3])
    ssb_low[,((j-1)*4+k)] <- sapply(1:ncol(ssb_re), function(x) boxplot.stats(ssb_re[,x])$`stats`[1])
    ssb_high[,((j-1)*4+k)] <- sapply(1:ncol(ssb_re), function(x) boxplot.stats(ssb_re[,x])$`stats`[5])
    
    r_median[,((j-1)*4+k)] <- sapply(1:ncol(r_re), function(x) boxplot.stats(r_re[,x])$`stats`[3])
    r_low[,((j-1)*4+k)] <- sapply(1:ncol(r_re), function(x) boxplot.stats(r_re[,x])$`stats`[1])
    r_high[,((j-1)*4+k)] <- sapply(1:ncol(r_re), function(x) boxplot.stats(r_re[,x])$`stats`[5])
    
    f_median[,((j-1)*4+k)] <- sapply(1:ncol(f_re), function(x) boxplot.stats(f_re[,x])$`stats`[3])
    f_low[,((j-1)*4+k)] <- sapply(1:ncol(f_re), function(x) boxplot.stats(f_re[,x])$`stats`[1])
    f_high[,((j-1)*4+k)] <- sapply(1:ncol(f_re), function(x) boxplot.stats(f_re[,x])$`stats`[5])
  }
}

#### Figure ####
jpeg(file="C:/Users/bai.li/Desktop/mcp_results_r/cases/manuscript_figures/SSB_R_F_RE.jpg", width=130, height=150, units="mm", res=300)

op<-par(no.readonly=TRUE)
par(op)
par(oma=c(5,5,0,5),mar=c(0,0,0,0.2),mfrow=c(9,3), pch=16)

x_val <- 1:length(ssb_median[,1])
ylim=c(-0.6, 0.6)

for (j in 1:length(maindir_list)){
  plot(x_val, rep(0, times=length(x_val)), ylim=ylim, type="l", lty=1, col="gray30", xlab="", ylab="", axes=F)
  box()
  axis(2, at=c(-0.4, 0, 0.4), labels=c(-0.4, 0, 0.4), las=2)
  if(j==length(maindir_list)) axis(1, at=c(5, 15, 25), labels = c(5, 15, 25))
  polygon(c(x_val, rev(x_val)), c(ssb_low[,(j-1)*4+1], rev(ssb_high[,(j-1)*4+1])), border=NA, col=alpha("orange", 0.2))
  #lines(x_val, ssb_median[,1], type="o", lty=1, col="orange", pch=2)
  
  polygon(c(x_val, rev(x_val)), c(ssb_low[,(j-1)*4+2], rev(ssb_high[,(j-1)*4+2])), border=NA, col=alpha("green", 0.2))
  #lines(x_val, ssb_median[,2], type="o", lty=1, col="green", pch=3)
  
  polygon(c(x_val, rev(x_val)), c(ssb_low[,(j-1)*4+3], rev(ssb_high[,(j-1)*4+3])), border=NA, col=alpha("red", 0.2))
  #lines(x_val, ssb_median[,3], type="o", lty=1, col="red", pch=4)
  
  polygon(c(x_val, rev(x_val)), c(ssb_low[,(j-1)*4+4], rev(ssb_high[,(j-1)*4+4])), border=NA, col=alpha("deepskyblue", 0.2))
  #lines(x_val, ssb_median[,4], type="o", lty=1, col="deepskyblue", pch=5)
  
  lines(x_val, ssb_median[,(j-1)*4+1], type="l", lty=2, col="orange")
  lines(x_val, ssb_median[,(j-1)*4+2], type="l", lty=3, col="green")
  lines(x_val, ssb_median[,(j-1)*4+3], type="l", lty=4, col="red")
  lines(x_val, ssb_median[,(j-1)*4+4], type="l", lty=5, col="deepskyblue")
  
  legend("topleft", paste("C", j-1, "_SSB", sep=""), bty="n")
  
  plot(x_val, rep(0, times=length(x_val)), ylim=ylim, type="l", lty=1, col="gray30", xlab="", ylab="", axes=F)
  box()
  if(j==length(maindir_list)) axis(1, at=c(5, 15, 25), labels = c(5, 15, 25))
  polygon(c(x_val, rev(x_val)), c(r_low[,(j-1)*4+1], rev(r_high[,(j-1)*4+1])), border=NA, col=alpha("orange", 0.2))
  #lines(x_val, r_median[,1], type="o", lty=1, col="orange", pch=2)
  
  polygon(c(x_val, rev(x_val)), c(r_low[,(j-1)*4+2], rev(r_high[,(j-1)*4+2])), border=NA, col=alpha("green", 0.2))
  #lines(x_val, r_median[,2], type="o", lty=1, col="green", pch=3)
  
  polygon(c(x_val, rev(x_val)), c(r_low[,(j-1)*4+3], rev(r_high[,(j-1)*4+3])), border=NA, col=alpha("red", 0.2))
  #lines(x_val, r_median[,3], type="o", lty=1, col="red", pch=4)
  
  polygon(c(x_val, rev(x_val)), c(r_low[,(j-1)*4+4], rev(r_high[,(j-1)*4+4])), border=NA, col=alpha("deepskyblue", 0.2))
  #lines(x_val, r_median[,4], type="o", lty=1, col="deepskyblue", pch=5)
  
  lines(x_val, r_median[,(j-1)*4+1], type="l", lty=2, col="orange")
  lines(x_val, r_median[,(j-1)*4+2], type="l", lty=3, col="green")
  lines(x_val, r_median[,(j-1)*4+3], type="l", lty=4, col="red")
  lines(x_val, r_median[,(j-1)*4+4], type="l", lty=5, col="deepskyblue")
  
  legend("topleft", paste("C", j-1, "_R", sep=""), bty="n")
  
  plot(x_val, rep(0, times=length(x_val)), ylim=ylim, type="l", lty=1, col="gray30", xlab="", ylab="", axes=F)
  box()
  if(j==length(maindir_list)) axis(1, at=c(5, 15, 25), labels = c(5, 15, 25))
  polygon(c(x_val, rev(x_val)), c(f_low[,(j-1)*4+1], rev(f_high[,(j-1)*4+1])), border=NA, col=alpha("orange", 0.2))
  #lines(x_val, f_median[,1], type="o", lty=1, col="orange", pch=2)
  
  polygon(c(x_val, rev(x_val)), c(f_low[,(j-1)*4+2], rev(f_high[,(j-1)*4+2])), border=NA, col=alpha("green", 0.2))
  #lines(x_val, f_median[,2], type="o", lty=1, col="green", pch=3)
  
  polygon(c(x_val, rev(x_val)), c(f_low[,(j-1)*4+3], rev(f_high[,(j-1)*4+3])), border=NA, col=alpha("red", 0.2))
  #lines(x_val, f_median[,3], type="o", lty=1, col="red", pch=4)
  
  polygon(c(x_val, rev(x_val)), c(f_low[,(j-1)*4+4], rev(f_high[,(j-1)*4+4])), border=NA, col=alpha("deepskyblue", 0.2))
  #lines(x_val, f_median[,4], type="o", lty=1, col="deepskyblue", pch=5)
  
  lines(x_val, f_median[,(j-1)*4+1], type="l", lty=2, col="orange")
  lines(x_val, f_median[,(j-1)*4+2], type="l", lty=3, col="green")
  lines(x_val, f_median[,(j-1)*4+3], type="l", lty=4, col="red")
  lines(x_val, f_median[,(j-1)*4+4], type="l", lty=5, col="deepskyblue")
  
  legend("topleft", paste("C", j-1, "_F", sep=""), bty="n")
}

mtext(text="Year",side=1,line=2.5,outer=TRUE)
mtext(text="RE",side=2,line=2.5,outer=TRUE)

legend(x=30.5,y=6,
       legend=c("AMAK", "ASAP", "BAM", "SS"),
       lty=c(2, 3, 4, 5), 
       col=c("orange", "green", "red", "deepskyblue3"), 
       cex=1, 
       bty="n", 
       xpd=NA)
dev.off()