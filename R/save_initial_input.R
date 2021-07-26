#' @export
## Save initial input values to a list
save_initial_input <- function(base_case=TRUE, input_list=NULL, ...){
  if (base_case==TRUE) {
   base_case <- NULL
   base_case <- list(
     maindir=maindir,
     om_sim_num=om_sim_num,
     keep_sim_num=keep_sim_num,
     figure_number=figure_number,

     seed_num=seed_num,
     stocks=input_list
   )
   edits <- list(...)
   base_case <- `[<-`(base_case, names(edits), edits)
   return(base_case)
  } else {
   edits <- list(...)
   update_input <- `[<-`(input_list, names(edits), edits)
   return(update_input)
  }
}
