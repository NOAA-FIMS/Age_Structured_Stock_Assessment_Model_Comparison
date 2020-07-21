check_convergence <- function(em_names, om_sim_num, col, plot_ncol, plot_nrow){
  positive_hessian = matrix(NA, ncol=length(em_names), nrow=om_sim_num)
  gradient = matrix(NA, ncol=length(em_names), nrow=om_sim_num)
  convergence_measures <- list(positive_hessian=positive_hessian, gradient=gradient)
  
  library(jsonlite)
  
  for (om_sim in 1:om_sim_num){
    for (em_id in 1:length(em_names)){
      subdir=em_names[em_id]
      setwd(file.path(maindir, "output", subdir, paste("s", om_sim, sep="")))
      
      if (subdir == "MAS"){
        
        convergence_measures$positive_hessian[om_sim, em_id] <- ifelse (file.exists(file.path(maindir, "output",  subdir, paste("s", om_sim, sep=""), paste("s", om_sim, ".json", sep=""))), 1, 0)
        
        load(file.path(maindir, "output", "OM", paste("OM", 1, ".RData", sep="")))
        mas_output <- read_json(file.path(maindir, "output",  subdir, paste("s", om_sim, sep=""), paste("s", om_sim, ".json", sep="")))
        popdy<-mas_output$population_dynamics
        pop<-popdy$populations[[1]]
        if (pop$MSY$F_msy > max(om_output$msy$f_seq) | 
            pop$MSY$F_msy < 0 |
            pop$females$MSY$SSB_msy > (om_output$msy$SSBmsy*2) | 
            pop$MSY$B_msy < 0
            ) convergence_measures$positive_hessian[om_sim, em_id] <- 0
        
        if (convergence_measures$positive_hessian[om_sim, em_id]==1) {
          json_output <- read_json(file.path(maindir, "output",  subdir, paste("s", om_sim, sep=""), paste("s", om_sim, ".json", sep="")))
          gradient_value <- unlist(json_output$estimated_parameters$parameters)
          convergence_measures$gradient[om_sim, em_id] <- max(as.numeric(gradient_value[which(names(gradient_value)=="gradient_value")]))
        } else {
          gradient_value <- NA
        }
        
      } else {
        parfile <- list.files(pattern="*.par")
        
        convergence_measures$positive_hessian[om_sim, em_id] <- ifelse (file.exists(file.path(maindir, "output",  subdir, paste("s", om_sim, sep=""), "admodel.cov")), 1, 0)
        convergence_measures$gradient[om_sim, em_id] <- ifelse(file.exists(file.path(maindir, "output",  subdir, paste("s", om_sim, sep=""), "admodel.cov")), as.numeric(scan(parfile, what='', n=16, quiet=TRUE)[c(6,11,16)])[3], NA)
      }
    }
  }

  save(convergence_measures, file=file.path(maindir, "output", "convergence_measures.RData"))

  not_positive_hessian <- unique(unlist(sapply(1:ncol(convergence_measures$positive_hessian), function(x) which(convergence_measures$positive_hessian[,x]==0))))
  write.csv(not_positive_hessian, file=file.path(maindir, "output", "Not_positive_hessian.csv"))
  
  if(max(convergence_measures$gradient, na.rm = T)<0.1){
    jpeg(file=file.path(maindir, "figure", "Gradient.jpg"))
    par(mfrow=c(plot_nrow, plot_ncol))
    xlim = c(0, max(convergence_measures$gradient, na.rm = T))
    bins <- seq(0, max(convergence_measures$gradient, na.rm = T)*2, by=0.0005)

    for (em_id in 1:length(em_names)){
      #hist(convergence_measures$gradient[,em_id], xlim=xlim, xlab = "Gradient", main="", col=col[em_id+1], breaks = bins)
      hist(convergence_measures$gradient[,em_id], xlim=xlim, xlab = "Gradient", main="", col=col[em_id+1], breaks = 5)
    # legend("topright", em_names[em_id], bty="n")
    box()
    }
    dev.off()
  }

  # keep_sim_id <<- c(1:om_sim_num)[-unique(c(
  # unlist(sapply(1:length(em_names), function(x) which(convergence_measures$gradient[,x] %in% boxplot.stats(convergence_measures$gradient[,x])$out))),
  # unique(unlist(sapply(1:ncol(convergence_measures$positive_hessian), function(x) which(convergence_measures$positive_hessian[,x]==0))))))][1:keep_sim_num]
  # 
  keep_sim_id <<- c(1:om_sim_num)[-unique(c(
    unlist(sapply(1:length(em_names), function(x) which(convergence_measures$gradient[,x] > 0.001))),
    unique(unlist(sapply(1:ncol(convergence_measures$positive_hessian), function(x) which(convergence_measures$positive_hessian[,x]==0))))))][1:keep_sim_num]


  if(max(convergence_measures$gradient[keep_sim_id,], na.rm = T)<0.1){
    jpeg(file=file.path(maindir, "figure", "Gradient_no_outliers.jpg"))
    par(mfrow=c(plot_nrow,plot_ncol))
    xlim = c(0, max(convergence_measures$gradient[keep_sim_id,], na.rm = T))
    bins <- seq(0, max(convergence_measures$gradient[keep_sim_id,], na.rm = T)*2, by=0.0005)
    for(em_id in 1:length(em_names)){
      #hist(convergence_measures$gradient[keep_sim_id,em_id], xlim=xlim, xlab = "Gradient", main="", col=col[em_id+1], breaks = bins)
      hist(convergence_measures$gradient[keep_sim_id,em_id], xlim=xlim, xlab = "Gradient", main="", col=col[em_id+1], breaks=5)
    legend("topright", em_names[em_id], bty="n")
    box()
    }
    dev.off()
  }

  keep_sim_num <<- length(keep_sim_id)
  save(keep_sim_id, om_sim_num, keep_sim_num, file=file.path(maindir, "output", "keep_sim_id.RData"))
}
