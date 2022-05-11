#' Save initial input values to a list
#'
#' The default is to return a list containing the values needed to
#' run case one, i.e., `"C1"`. This function can also be used to modify
#' the list elements using `...` rather than assigning each change with `<-`.
#'
#' @param base_case A logical value to declare if you want the default values
#'   that are stored inside this function for case 1, i.e., `"C1"`.
#' @param input_list If `base_case = FALSE`, then `input_list` must be provided.
#'   The argument can take any list but a list previously returned from
#'   `save_initial_input()` is ideal such that it has all of the necessary
#'   elements.
#' @param maindir The directory where you the results from down-stream
#'   functions like [run_OM()] will save the results to.
#'   TODO - decide if this argument should be added and how to force it to
#'          make changes because it will not be included in ...
#'        - decide if we should use dir.create() to ensure that the directory
#'          exists
#'        - is the current directory okay as a default or should it be
#'          a temporary directory?
#' @param ... Elements of `input_list` or the returned list using the base-case
#'   scenario if `base_case = TRUE` that you want to change.
#' @author Bai Li
#' @export
#' @seealso [run_OM()]
#' @return A list is object is returned. Each element is necessary to run a
#' simulation via passing the list to [run_OM()].
#' TODO - use the saved data object to generate a bulleted list of names(C1).
#'      - double check all of the input values
#' @examples
#' # Create the default input for case one, i.e., "C1"
#' null_case_input <- save_initial_input()
#' # Change the default to case two, i.e., "C2"
#' updated_input <- save_intial_input(
#'   base_case = FALSE,
#'   input_list = null_case_input,
#'   case_name = "C2",
#'   logR_sd = 0.6
#' )
#'
save_initial_input <- function(base_case=TRUE,
                               input_list=NULL,
                               maindir = getwd(),
                               ...) {
  if (base_case == TRUE) {
    base_case <- list(
      maindir = maindir,
      case_name = "C1",
      om_sim_num = 120,
      keep_sim_num = 100,
      figure_number = 10,
      seed_num = 9924,

      year = 1:30,
      ages = 1:12,
      initial_equilibrium_F = TRUE,
      median_R0 = 1000000,
      median_h = 0.75,
      mean_R0 = NULL,
      mean_h = NULL,
      SRmodel = 1, # 1:Beverton-Holt; 2:Ricker
      M = 0.2,
      Linf = 800,
      K = 0.18,
      a0 = -1.36,
      a.lw = 0.000000025,
      b.lw = 3.0,
      A50.mat = 2.25,
      slope.mat = 3,
      pattern.mat = 1,
      female.proportion = 0.5,

      fleet_num = 1,
      cv.L = list(fleet1 = 0.005),
      input.cv.L = list(fleet1 = 0.01),
      n.L = list(fleet1 = 200),
      sel_fleet = list(fleet1 = list(
        pattern = 1,
        A50.sel1 = 2,
        slope.sel1 = 1
      )),
      survey_num = 1,
      cv.survey = list(survey1 = 0.1),
      input.cv.survey = list(survey1 = 0.2),
      n.survey = list(survey1 = 200),
      sel_survey = list(survey1 = list(
        pattern = 1,
        A50.sel1 = 1.5,
        slope.sel1 = 2
      )),

      logf_sd = 0.2,
      f_dev_change = FALSE,
      f_pattern = 1,
      start_val = 0.01,
      middle_val = NULL,
      end_val = 0.39,
      f_val = NULL,
      start_year = 1,
      middle_year = NULL,

      logR_sd = 0.4,
      r_dev_change = TRUE,

      om_bias_cor = FALSE,
      bias_cor_method = "none", # c("none", "median_unbiased", "mean_unbiased")
      em_bias_cor = FALSE
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
