plot_stock_status_determination <- function(em_names, col){
  comparison_var <- c("fratio", "ssbratio")
  comparison_id <- which(names(om_list) %in% comparison_var)

  overfishing_data <- list()
  overfishing_accuracy <- matrix(NA, nrow=length(year), ncol=length(em_names))
  for (k in 1:length(em_names)){
    overfishing_data[[k]] <- matrix(NA, nrow=length(year), ncol=keep_sim_num)
    for(j in 1:keep_sim_num){
      for (i in 1:length(year)){
        if((em_list[[k]][[comparison_id[1]]][i,j]<1 & om_list$fratio[i,j]<1) | (em_list[[k]][[comparison_id[1]]][i,j]>1 & om_list$fratio[i,j]>1)){
          overfishing_data[[k]][i,j] = 1
        } else{
          overfishing_data[[k]][i,j] = 0
        }
      }
    }
    overfishing_accuracy[,k] <- apply(overfishing_data[[k]], 1, sum)/keep_sim_num*100
  }
  colnames(overfishing_accuracy) <- em_names
  rownames(overfishing_accuracy) <- year
  write.csv(overfishing_accuracy, file=file.path(maindir, "output", "overfishing_accuracy.csv"))

  overfished_data <- list()
  overfished_accuracy <- matrix(NA, nrow=length(year), ncol=length(em_names))
  for (k in 1:length(em_names)){
    overfished_data[[k]] <- matrix(NA, nrow=length(year), ncol=keep_sim_num)
    for(j in 1:keep_sim_num){
      for (i in 1:length(year)){
        if((em_list[[k]][[comparison_id[1]]][i,j]<1 & om_list$fratio[i,j]<1) | (em_list[[k]][[comparison_id[1]]][i,j]>1 & om_list$fratio[i,j]>1)){
          overfished_data[[k]][i,j] = 1
        } else{
          overfished_data[[k]][i,j] = 0
        }
      }
    }
    overfished_accuracy[,k] <- apply(overfished_data[[k]], 1, sum)/keep_sim_num*100
  }
  colnames(overfished_accuracy) <- em_names
  rownames(overfished_accuracy) <- year
  write.csv(overfished_accuracy, file=file.path(maindir, "output", "overfished_accuracy.csv"))

}
