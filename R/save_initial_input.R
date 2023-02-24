#' Save initial input values to a list
#'
#' @description
#' The default is to return a list containing the values needed to
#' run case one, i.e., `"C1"`. This function can also be used to modify
#' the list elements using `...` rather than assigning each change with `<-`.
#'
#' @details
#' The elements of `input`
#'
#' @param base_case A logical value to declare if you want the default values
#'   that are stored inside this function for case 1, i.e., `"C1"`.
#' @param input_list If `base_case = FALSE`, then `input_list` must be provided.
#'   The argument can take any list but a list previously returned from
#'   `save_initial_input()` is ideal such that it has all of the necessary
#'   elements.
#' @param maindir The directory where you the results from down-stream
#'   functions like \link{run_om} will save the results to.
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
#' @seealso \link{run_om}
#' @return A list is object is returned. Each element is necessary to run a
#' simulation via passing the list to \link{run_om}.
#' TODO - use the saved data object to generate a bulleted list of names(C1).
#'      - double check all of the input values
#' \describe{
#'   \item{om_inupt}{A list of input data of the operating model.}
#'   \item{om_inupt$fleet_num}{Number of fleets.}
#'   \item{om_inupt$survey_num}{Number of surveys.}
#'   \item{om_inupt$nyr}{Number of years.}
#'   \item{om_inupt$year}{A vector of model years.}
#'   \item{om_inupt$ages}{A vector of ages of the population.}
#'   \item{om_inupt$nage}{Number of ages of the population.}
#'   \item{om_inupt$cv.L}{CV of landings.}
#'   \item{om_inupt$cv.survey}{CV of surveys.}
#'   \item{om_inupt$n.L}{Annual sample size (i.e., number of fish) of landing age composition samples.}
#'   \item{om_inupt$n.survey}{Annual sample size (i.e., number of fish) of survey age composition samples.}
#'   \item{om_inupt$logR_sd}{Standard deviation of log recruitment.}
#'   \item{om_inupt$logf_sd}{Standard deviation of log fully selected fishing mortality.}
#'   \item{om_inupt$om_bias_cor}{Logical. Apply bias correction to recruitment if TRUE. Default value is FALSE.}
#'   \item{om_inupt$bias_cor_method}{Bias correction method. Options include "none", "median_unbiased", and "mean_unbiased".}
#'   \item{om_inupt$R0}{Input of unfished recruitment in number.}
#'   \item{om_inupt$h}{Input of steepness of the stock-recruit model.}
#'   \item{om_inupt$median_R0}{Median-unbiased unfished recruitment in number.}
#'   \item{om_inupt$median_h}{Median-unbiased steepness of the stock-recruit model.}
#'   \item{om_inupt$mean_R0}{Mean-unbiased unfished recruitment in number.}
#'   \item{om_inupt$mean_h}{Mean-unbiased steepness of the stock-recruit model.}
#'   \item{om_inupt$SRmodel}{Stock-recruit model. 1 = Beverton-Holt model; 2 = Ricker model.}
#'   \item{om_inupt$M}{Age-invariant natural mortality.}
#'   \item{om_inupt$Linf}{Asymptotic average length (mm).}
#'   \item{om_inupt$K}{Growth coefficient.}
#'   \item{om_inupt$a0}{Age at mean length of zero.}
#'   \item{om_inupt$a.lw}{Length-weight coefficient.}
#'   \item{om_inupt$b.lw}{Length-weight exponent.}
#'   \item{om_inupt$A50.mat}{Age at 50% maturity.}
#'   \item{om_inupt$slope.mat}{Slope of maturity ogive.}
#'   \item{om_inupt$om_inupt$sel_fleet}{Fleet selectivity list. The list include selectivity pattern information (1 = simple logistic; 2 = double logistic).}
#'   \item{om_inupt$sel_survey}{Survey selectivity list. The list include selectivity pattern information (1 = simple logistic; 2 = double logistic).}
#'   \item{om_inupt$len}{Length-at-age vector from von Bertalanffy growth model.}
#'   \item{om_inupt$W.kg}{Weight-at-age in kg.}
#'   \item{om_inupt$W.mt}{Weight-at-age in mt.}
#'   \item{om_inupt$M.age}{Natural mortality-at-age.}
#'   \item{om_inupt$mat.age}{Maturity-at-age.}
#'   \item{om_inupt$proportion.female}{Proportion of female fish.}
#'   \item{om_inupt$selex_fleet}{Selectivity-at-age of fleet.}
#'   \item{om_inupt$selex_survey}{Selectivity-at-age of survey.}
#'   \item{om_inupt$N.pr0}{Number of spawners per recruit-at-age.}
#'   \item{om_inupt$Phi.0}{Spawners per recruit based on mature female biomss.}
#'   \item{om_inupt$logR.resid}{Annual recruitment deviations.}
#'   \item{om_inupt$logf.resid}{Annual fishing mortality deviations.}
#'   \item{om_inupt$f}{Annual fully selected fishing mortality.}
#'   \item{om_inupt$initial_equilibrium_F}{Logic. Set initial condition to the unfished equilibrium population or not.}
#'
#'   \item{om_output}{A list of output data of the operating model.}
#'   \item{om_output$year}{A vector of model years.}
#'   \item{om_output$SSB}{Annual Spawning stock biomass in mt.}
#'   \item{om_output$abundance}{Annual abundance in number.}
#'   \item{om_output$biomass.mt}{Annual biomass in mt.}
#'   \item{om_output$N.age}{Abundance-at-age a (column) in year y(row) (in number).}
#'   \item{om_output$L.age}{Landing-at-age a (column) in year y (row) (in number).}
#'   \item{om_output$L.knum}{Landing-at-age a (column) in year y (row) (in 1000 number).}
#'   \item{om_output$L.mt}{Landing-at-age a (column) in year y (row) (in mt).}
#'   \item{om_output$msy}{A list of reference points. The list include maximum sustainable yield (msy), fishing rate at MSY (FMSY), dead discards at MSY (Dmsy), spawner per recruit at msy (spr_msy), spawning potential ratio (SPRmsy = spr_msy/spr_virgin), Spawning stock biomass at MSY (SSBmsy), equilibrium recruitment at MSY (Rmsy), total biomass (male and female fish) at MSY (Bmsy), exploitation rate at MSY (Emsy), a sequence of fishing mortality for searching MSY (f_seq), yield at each fishing mortality value (L_eq), discard at each fishing mortality value (D_eq), spawning stock biomass at each fishing mortality value (SSB_eq), recruitment at each fishing mortality value (R_eq), spawner per recruit at each fishing mortality value (spr), maximum fishing mortality value examined (maxF), accuracy in MSY calculations (step), lognormal bias correction (sigma), constant vector multiplied by abudance to get spawning stock biomass at age (reprod). }
#'   \item{om_output$f}{Annual fully selected fishing mortality.}
#'   \item{om_output$FAA}{Fishing mortality-at-age a (column) in year y (row).}
#'   \item{om_output$survey_age_comp}{Survey age composition data with no observation error. Columns represent ages and rows represent years.}
#'   \item{om_output$survey_index}{Scaled survey index with no observation error.}
#'   \item{om_output$survey_q}{Survey catchability.}
#'
#'   \item{em_input}{Observed data with observation errors.}
#'   \item{em_input$L.obs}{Observed annual landings in mt with observation error.}
#'   \item{survey.obs}{Observed annual survey index (scaled) with observation error.}
#'   \item{L.age.obs}{Observed landing age composition data in mt with observation error.}
#'   \item{survey.age.obs}{Observed survey age composition data in proportion with observation error.}
#'   \item{n.L}{Annual sample size (i.e., number of fish) of landing age composition samples.}
#'   \item{n.survey}{Annual sample size (i.e., number of fish) of survey age composition samples.}
#'   \item{survey_q}{Survey catchability.}
#'   \item{cv.L}{CV of landings.}
#'   \item{cv.survey}{CV of surveys.}
#'
#'
#'
#' }
#' @examples
#' \dontrun{
#' # Create the default input for case one, i.e., "C1"
#' null_case_input <- save_initial_input()
#' # Change the default to case two, i.e., "C2"
#' updated_input <- save_intial_input(
#'   base_case = FALSE,
#'   input_list = null_case_input,
#'   case_name = "C2",
#'   logR_sd = 0.6
#' )
#'}
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
