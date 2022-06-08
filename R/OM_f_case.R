#' Compute fishing mortality rate over years.
#'
#' @param f_pattern A pattern of fishing mortality rate.
#' f_pattern: 1. increase
#'            2. decrease
#'            3. increase first, then decrease
#'            4. decrease first, then increase
#'            5. constantly fluctuate around a value
#'            6. customized pattern
#' @param start_val start value of the main shape of fishing mortality
#' @param middle_val middle value of the main shape of fishing mortality for case 3 and 4
#' @param end_val end value of the main shape of fishing mortality for case 3 and 4
#' @param f_val customized annual value of the main shape of fishing mortality
#' @param start_year start year for the start value of fishing mortality
#' @param middle_year middle year for the middle value of fishing mortality
#' @param nyr total number of years
#' @param om_sim_num total iteration numbers
#' @param f_dev_matrix F deviations-at-age per iteration
#' @return A matrix of fishing mortality rate \code{f_matrix} over years (column) for each iteration (row)
#' @examples
#' \dontrun{
#' f_case(f_pattern=1, start_val=0.01, middle_val=NULL, end_val=0.39, f_val=NULL, start_year=1, middle_year=NULL, nyr=30, om_sim_num=160, f_dev_matrix=f_dev_matrix)
#' f_case(f_pattern=3, start_val=0.01, middle_val=0.39, end_val=0.19, f_val=NULL, start_year=1, middle_year=24, nyr=30, om_sim_num=160, f_dev_matrix=f_dev_matrix)
#' f_case(f_pattern=5, start_val=0.01, middle_val=NULL, end_val=NULL, f_val=NULL, start_year=1, middle_year=NULL, nyr=30, om_sim_num=160, f_dev_matrix=f_dev_matrix)
#' f_case(f_pattern=6, f_val=rnorm(n=30, mean=1, sd=0.3), nyr=30, om_sim_num=160, f_dev_matrix=f_dev_matrix)
#' }
#' @export


f_case <- function(f_pattern=1,
                   start_val=0.01, middle_val=0.39, end_val=NULL,
                   f_val=NULL,
                   start_year=NULL, middle_year=NULL,
                   nyr=30,
                   om_sim_num=160,
                   f_dev_matrix=f_dev_matrix,
                   initial_equilibrium_F=TRUE){

  f_matrix <- matrix(NA, nrow=om_sim_num, ncol=nyr)

  if(initial_equilibrium_F==FALSE){
    col_id <- 2:ncol(f_matrix)
    f_matrix[,1] <- rep(0, nrow(f_matrix))
    nyr=nyr-1
  } else {
    col_id <- 1:ncol(f_matrix)
  }

  for(om_sim in 1:om_sim_num){
    if(f_pattern %in% c(1,2)) f_matrix[om_sim,col_id] <- seq(start_val, end_val, length=nyr)*exp(f_dev_matrix[om_sim,col_id])

    if(f_pattern %in% c(3,4)) f_matrix[om_sim,col_id] <- c(seq(start_val, middle_val, length=middle_year), seq(middle_val, end_val, length=nyr-middle_year))*exp(f_dev_matrix[om_sim,col_id])

    if(f_pattern == 5) f_matrix[om_sim,col_id] <- c(rep(start_val, length=middle_year), rep(middle_val, length=nyr-middle_year))*exp(f_dev_matrix[om_sim,col_id])

    if(f_pattern == 6) f_matrix[om_sim,col_id] <- f_val*exp(f_dev_matrix[om_sim,col_id])
  }

  return(f_matrix)
}
