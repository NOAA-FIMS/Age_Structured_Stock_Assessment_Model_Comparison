#' Recruitment deviations generator
#' @param rec_dev_change if True, generate different recruitment deviations per iteration; If FALSE, generate same recruitment deviations per iteration
#' @param nyr number of years
#' @param logR.sd recruitment SD in log space
#' @param om_sim_num number of iterations per case
#' @param r_dev_sum2zero Sum recruitment deviations to 0 or not
#' @export
r_dev_case <- function(r_dev_change=TRUE, nyr=30, logR_sd=0.1, om_sim_num=160, r_dev_sum2zero=FALSE){

  r_dev_matrix <- matrix(NA, nrow=om_sim_num, ncol=nyr)

  for(om_sim in 1:om_sim_num){
    if(r_dev_change==TRUE){
      r_dev_matrix[om_sim,] <- rnorm(nyr, mean=0, sd=logR_sd)
      if (r_dev_sum2zero == TRUE) r_dev_matrix[om_sim,] <- r_dev_matrix[om_sim,] - mean(r_dev_matrix[om_sim,])

    } else {
      if(om_sim==1){
        r_dev_matrix[om_sim,] <- rnorm(nyr, mean=0, sd=logR_sd)
        if (r_dev_sum2zero == TRUE) r_dev_matrix[om_sim,] <- r_dev_matrix[om_sim,] - mean(r_dev_matrix[om_sim,])
      } else {
        r_dev_matrix[om_sim,] <- r_dev_matrix[(om_sim-1),]
      }
    }
  }

  return(r_dev_matrix)
}

#' F deviations generator
#' @param f_dev_change if True, generate different F deviations per iteration; If FALSE, generate same F deviations per iteration
#' @param nyr number of years
#' @param logF.sd SD of fishing mortality in log space
#' @param om_sim_num number of iterations per case
#' @param f_dev_sum2zero Sum F deviations to 0 or not
#' @export
f_dev_case <- function(f_dev_change, nyr=30, logf_sd=0.1, om_sim_num=160, f_dev_sum2zero = FALSE){

  f_dev_matrix <- matrix(NA, nrow=om_sim_num, ncol=nyr)

  for(om_sim in 1:om_sim_num){

    if(f_dev_change==TRUE){
      f_dev_matrix[om_sim,] <- rnorm(nyr, mean=0, sd=logf_sd)
      if (f_dev_sum2zero == TRUE) f_dev_matrix[om_sim,] <- f_dev_matrix[om_sim,] - mean(f_dev_matrix[om_sim,])
    } else {
      if(om_sim==1){
        f_dev_matrix[om_sim,] <- rnorm(nyr, mean=0, sd=logf_sd)
        if (f_dev_sum2zero == TRUE) f_dev_matrix[om_sim,] <- f_dev_matrix[om_sim,] - mean(f_dev_matrix[om_sim,])
      } else {
        f_dev_matrix[om_sim,] <- f_dev_matrix[(om_sim-1),]
      }
    }
  }

  return(f_dev_matrix)
}

