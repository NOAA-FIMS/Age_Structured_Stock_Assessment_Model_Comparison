run_mas <- function(maindir=maindir, subdir="MAS", om_sim_num=NULL){
  subdir <<- "MAS"
  setwd(file.path(maindir, "output", subdir))
  unlink(list.files(file.path(maindir, "output", "MAS"), full.names = TRUE), recursive = TRUE)
  sapply(1:om_sim_num, function(x) dir.create(file.path(maindir, "output", subdir, paste("s", x, sep=""))))
  save.image(file=file.path(maindir, "output", subdir, "myEnvironment.RData"))

  om_sim <- 1
  save(om_sim, file=file.path(maindir, "output", subdir, "om_sim.RData"))

  source("C:/Users/bai.li/Documents/Github/Age_Structured_Stock_Assessment_Model_Comparison/ASSAMC/example/em_input/mas.R")

}
