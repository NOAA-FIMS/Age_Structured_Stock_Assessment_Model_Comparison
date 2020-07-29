check_performance <- function (em_names){
  comparison_var <- c("biomass", "abundance", "ssb", "recruit", "Ftot", "landing", "survey", "msy", "fmsy", "ssbmsy", "fratio", "ssbratio")
  comparison_id <- which(names(om_list) %in% comparison_var)

  em_list <- list()
  for(i in 1:length(em_names)){
    em_list[[i]] <- eval(as.name(tolower(paste(em_names, "_list", sep="")[i])))
  }
  em_list <<- em_list

  ## Relative error (RE)
  re_list <- list()
  for(k in 1:keep_sim_num){
    re_var <- list()
    for(j in 1:length(comparison_id)){
      re <- as.data.frame(matrix(NA, ncol=length(em_names), nrow=length(om_list[[comparison_id[j]]][,k])))
      for (i in 1:length(em_names)){
        re[,i] <- (em_list[[i]][[j]][,k] - om_list[[comparison_id[j]]][,k]) / om_list[[comparison_id[j]]][,k]
      }
      colnames(re) <- em_names
      re_var[[j]] <- re
    }
    names(re_var) <- comparison_var
    re_list[[k]] <- re_var
  }
  re_list <<- re_list

  ## Absolute relative error (ARE)
  are_list <- list()
  for(k in 1:keep_sim_num){
    are_var <- list()
    for(j in 1:length(comparison_id)){
      are <- as.data.frame(matrix(NA, ncol=length(em_names), nrow=length(om_list[[comparison_id[j]]][,k])))
      for (i in 1:length(em_names)){
        are[,i] <- (em_list[[i]][[j]][,k] - om_list[[comparison_id[j]]][,k]) / om_list[[comparison_id[j]]][,k]
      }
      colnames(are) <- em_names
      are_var[[j]] <- are
    }
    names(are_var) <- comparison_var
    are_list[[k]] <- are_var
  }
  are_list <<- are_list

  ## Correlation coefficient (CC)
  cc_list <- list()
  for(k in 1:keep_sim_num){
    cc_var <- list()
    for(j in 1:length(comparison_id)){
      cc <- as.data.frame(NA, ncol=length(em_names), nrow=1)
      for (i in 1:length(em_names)){
        cc[,i] <- sum((om_list[[comparison_id[j]]][,k] - mean(om_list[[comparison_id[j]]][,k])) *
          (em_list[[i]][[j]][,k] - mean(em_list[[i]][[j]][,k]))) /
          sqrt(sum((om_list[[comparison_id[j]]][,k] - mean(om_list[[comparison_id[j]]][,k]))^2) *
                 sum((em_list[[i]][[j]][,k] - mean(em_list[[i]][[j]][,k]))^2))
      }
      colnames(cc) <- em_names
      cc_var[[j]] <- cc
    }
    names(cc_var) <- comparison_var
    cc_list[[k]] <- cc_var
  }
  cc_list <<- cc_list

  ## Compile performance measure results
  save(re_list, are_list, cc_list, file=file.path(maindir, "output", "performance_measure.RData"))
}

