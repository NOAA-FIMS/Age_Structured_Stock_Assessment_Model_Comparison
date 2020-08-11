library(RColorBrewer)
maindir_list  <- c("C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case0",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case1",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case2",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case3",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case4",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case5",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case6",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case7",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case8",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case9",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case10",
                   "C:/Users/bai.li/Desktop/mcp_results_r/final_cases/case11")


yr=1:30
em_num <- 4

overfishing_accuracy <- overfished_accuracy <- matrix (NA, nrow=em_num*length(maindir_list), ncol=length(yr))


for (j in 1:length(maindir_list)){
  overfishing_accuracy[((j-1)*4+1):((j-1)*4+4), ] <- t(as.matrix(read.csv(file.path(maindir_list[j], "output", "overfishing_performance.csv"))[, 2:(1+em_num)]))
  overfished_accuracy[((j-1)*4+1):((j-1)*4+4), ] <- t(as.matrix(read.csv(file.path(maindir_list[j], "output", "overfished_performance.csv"))[, 2:(1+em_num)]))
}

library(ComplexHeatmap)
row_names = rep(c("AMAK", "ASAP", "BAM", "SS"), times=length(maindir_list))
ht1 <- Heatmap(overfishing_accuracy,
               col = colorRampPalette(brewer.pal(n = 9, name = "YlGnBu"))(100),
               #col = colorRampPalette(brewer.pal(n = 11, name = "Spectral"))(100),
               rect_gp = gpar(col = "gray"),
               cluster_rows = FALSE,
               cluster_columns = FALSE,
               row_labels = row_names,
               row_names_side = c("right"),
               show_row_names = TRUE,
               row_names_max_width = unit(6, "cm"),
               row_names_gp = gpar(fontsize = 8),
               column_labels = yr,
               column_names_side = c("bottom"),
               show_column_names = TRUE,
               column_names_max_height = unit(6, "cm"),
               column_names_gp = gpar(fontsize = 8),
               heatmap_legend_param = list(
                 title = "Accuracy (%)")
)

jpeg(file="C:/Users/bai.li/Desktop/mcp_results_r/cases/manuscript_figures/overfishing_accuracy.jpg", width=150, height=150, units="mm", res=300)
draw(ht1,
     row_split = rep(paste("C", 0:(length(maindir_list)-1), sep=""), each = 4))
dev.off()


#### Line plot ####
jpeg(file="C:/Users/bai.li/Desktop/mcp_results_r/cases/manuscript_figures/overfishing_accuracy_line.jpeg", width=150, height=120, units="mm", res=300)

op<-par(no.readonly=TRUE)
par(op)
par(oma=c(4,4,0,5),mar=c(0.5,0,0.1,0.5),mfrow=c(4,3),pch=16)

cases = c("case0", "case1", "case2", "case3", "case4", "case5", "case6", "case7", "case8", "case9", "case10_BAM", "case11_SS")

for (i in 1:length(cases)){
  overfishing = read.csv(paste("C:/Users/bai.li/Desktop/mcp_results_r/final_cases/", cases[i], "/output/overfishing_performance.csv", sep=""))
  plot(overfishing$X, overfishing[,2], col=alpha("orange", 0.7), ylim=c(0, 100), type="o", xlab="", ylab="", axes=F, lty=2, pch=2, cex=0.5)
  box()
  if (i %in% c(length(cases):(length(cases)-2))) axis(1)
  if(i %in% seq(1, length(cases), by=3)) axis(2)
  lines(overfishing$X, overfishing[,3], col=alpha("green", 0.7), lty=3, pch=3, type="o", cex=0.5)
  lines(overfishing$X, overfishing[,4], col=alpha("red", 0.7), lty=4, pch=4, type="o", cex=0.5)
  lines(overfishing$X, overfishing[,5], col=alpha("deepskyblue3", 0.7), lty=5, pch=5, type="o", cex=0.5)
  legend("bottomleft", paste("C", i-1, sep=""), bty="n")
}
mtext(text="Year",side=1,line=2,outer=TRUE)
mtext(text="F/Flimit Accuracy (%)",side=2,line=2,outer=TRUE)

legend(31, 200,
       c("AMAK", "ASAP", "BAM", "SS"),
       lty=c(2,3,4,5),
       pch=c(2,3,4,5),
       bty="n",
       cex=1,
       col=c("orange", "green", "red", "deepskyblue3"),
       xpd=NA)
dev.off()


