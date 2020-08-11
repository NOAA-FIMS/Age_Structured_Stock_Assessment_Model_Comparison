run_bam <- function(maindir=maindir, subdir="BAM", om_sim_num=NULL, casedir=casedir, em_bias_cor=em_bias_cor){
  if(!("readxl" %in% installed.packages()[,"Package"])) install.packages("readxl")
  library(readxl)

  setwd(file.path(casedir, "output", subdir))
  unlink(list.files(file.path(casedir, "output", "BAM"), full.names = TRUE), recursive = TRUE)
  sapply(1:om_sim_num, function(x) dir.create(file.path(casedir, "output", subdir, paste("s", x, sep=""))))

  bam_parms=read_xlsx(path=file.path(maindir, "em_input", "BAM.DatInput.Parms.xlsx"))

  modify_input = "partial"
  for (om_sim in 1:om_sim_num){
    load(file=file.path(casedir, "output", "OM", paste("OM", om_sim, ".RData", sep="")))

    if(modify_input == "all") {

    } #incomplete
    if (modify_input == "partial"){
      bam_parms[bam_parms$notes=="sd of recruitment in log space",c(1,5)]=om_input$logR_sd
      #bam_parms[bam_parms$notes=="catchability of survey",c(1,5)]=log(em_input$survey_q$survey1)
      bam_parms[bam_parms$notes=="log mean F",c(1,5)]=log(mean(om_output$f))

      dat.survey1=list(nyr=length(em_input$survey.obs$survey1),
                       yrs=om_input$year,
                       vals.obs=em_input$survey.obs$survey1,
                       cv=rep(em_input$cv.survey$survey1, length(em_input$survey.obs$survey1)),
                       nyr.ages=length(em_input$survey.obs$survey1),
                       yrs.age=om_input$year,
                       nsamp=rep(em_input$n.survey$survey1, length(em_input$survey.obs$survey1)),
                       nfish=rep(em_input$n.survey$survey1, length(em_input$survey.obs$survey1)),
                       acomp=em_input$survey.age.obs$survey1)
      if (om_input$survey_num==2) {
        dat.survey2=list(nyr=length(em_input$survey.obs$survey2),
                          yrs=om_input$year,
                          vals.obs=em_input$survey.obs$survey2,
                          cv=rep(em_input$cv.survey$survey2, length(em_input$survey.obs$survey2)),
                          nyr.ages=length(em_input$survey.obs$survey2),
                          yrs.age=om_input$year,
                          nsamp=rep(em_input$n.survey$survey2, length(em_input$survey.obs$survey2)),
                          nfish=rep(em_input$n.survey$survey2, length(em_input$survey.obs$survey2)),
                          acomp=em_input$survey.age.obs$survey2)
      }


      dat1.L=list(styr=om_input$year[1],
                  endyr=om_input$year[length(om_input$year)],
                  vals.obs=em_input$L.obs$fleet1,
                  cv=rep(em_input$cv.L$fleet1, length(em_input$L.obs$fleet1)),
                  nyr.ages=length(em_input$L.obs$fleet1),
                  yrs.age=om_input$year,
                  nsamp=rep(em_input$n.L$fleet1, length(em_input$L.obs$fleet1)),
                  nfish=rep(em_input$n.L$fleet1, length(em_input$L.obs$fleet1)),
                  acomp=em_input$L.age.obs$fleet1)
      if (om_input$survey_num==1){
       BAM.write.dat(fname=file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.dat"),
                     nyr=om_input$nyr,
                     nages=om_input$nages,
                     dat.survey1=dat.survey1,
                     dat.L=dat1.L,
                     parms=bam_parms,
                     a.lw=om_input$a.lw,
                     b.lw=om_input$b.lw,
                     prop.f=om_input$proportion.female,
                     mat.age=om_input$mat.age,
                     M.age=om_input$M.age,
                     em_bias_cor=em_bias_cor,
                     SRmodel=om_input$SRmodel)
      }

      if (om_input$survey_num==2) {
       BAM.write.dat(fname=file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.dat"),
                     nyr=om_input$nyr,
                     nages=om_input$nages,
                     dat.survey1=dat.survey1,
                     dat.survey2=dat.survey2,
                     dat.L=dat1.L,
                     parms=bam_parms,
                     a.lw=om_input$a.lw,
                     b.lw=om_input$b.lw,
                     prop.f=om_input$proportion.female,
                     mat.age=om_input$mat.age,
                     M.age=om_input$M.age,
                     em_bias_cor=em_bias_cor,
                     SRmodel=om_input$SRmodel)
      }
    }
  }

  for (om_sim in 1:om_sim_num){
    setwd(file.path(casedir, "output", subdir, paste("s", om_sim, sep="")))
    file.copy(file.path(maindir, "em_input", "BAM-Sim.exe"), file.path(casedir,"output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.exe"), overwrite = T)
    system(paste(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.exe"), file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.dat"), sep = " "), show.output.on.console = FALSE)
    file.remove(file.path(casedir, "output", subdir, paste("s", om_sim, sep=""), "BAM-Sim.exe"))
  }
}
