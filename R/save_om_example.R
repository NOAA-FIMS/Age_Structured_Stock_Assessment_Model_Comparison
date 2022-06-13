#' Save output from [run_om()] to use in examples and documentation
#'
#' Save the output from an example call to [run_om()] as a list.
#' `save_om_example()` gets the base-case inputs, sets the working directory,
#' cleans up the workspace, and returns your workspace to the original
#' directory.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A list of three larger lists,
#' * `om_input`: input information provided to [run_om()],
#' * `om_output`: simulated output from a single iteration of the
#'   operating model, and
#' * `em_input`: a combination of `om_input`, `om_output`, and information
#'   needed to run an estimation model.
#' @examples
#' \dontrun{
#' omsimlist <- save_om_example()
#' # Check out the mega list of 3 lists
#' str(omsimlist, max.level = 2)
#' # Clean up your workspace
#' rm(omsimlist)
#' }
save_om_example <- function() {
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  null_case_input <- save_initial_input(
    base_case = TRUE,
    maindir = tempdir()
  )
  null_case_input[["om_sim_num"]] <- 1
  null_case_input[["keep_sim_num"]] <- 1
  null_case_input[["figure_number"]] <- 1
  run_om(input_list = null_case_input, show_iter_num = T)
  newdir <- dir(pattern = "C1", full.names = TRUE)
  valuetoreturn <- list(
    om_input = om_input,
    om_output = om_output,
    em_input = em_input
  )
  on.exit(unlink(newdir, recursive = TRUE), add = TRUE)
  on.exit(
    rm(
      em_input, om_input, om_output,
      envir = .GlobalEnv
    ),
    add = TRUE
  )
  return(valuetoreturn)
}
