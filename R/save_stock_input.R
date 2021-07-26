#' @export
## Save initial input values of a stock to a list
save_stock_input <- function(base_stock=TRUE, input_list=NULL, ...){
  if (base_stock==TRUE) {
    base_stock <- NULL
    base_stock <- list(recruit_transportation=recruit_transportation,
                       movement_matrix=movement_matrix,

                       year=year,
                       ages=ages,
                       initial_equilibrium_F=initial_equilibrium_F,
                       median_R0=median_R0,
                       median_h=median_h,
                       mean_R0=mean_R0,
                       mean_h=mean_h,
                       SRmodel=SRmodel,
                       M=M,
                       Linf=Linf,
                       K=K,
                       a0=a0,
                       a.lw=a.lw,
                       b.lw=b.lw,
                       A50.mat=A50.mat,
                       slope.mat=slope.mat,
                       pattern.mat=pattern.mat,
                       female.proportion=female.proportion,

                       fleet_num=fleet_num,
                       cv.L=cv.L,
                       input.cv.L=input.cv.L,
                       n.L=n.L,
                       sel_fleet=sel_fleet,

                       survey_num=survey_num,
                       cv.survey=cv.survey,
                       input.cv.survey=input.cv.survey,
                       n.survey=n.survey,
                       sel_survey=sel_survey,

                       brp_f_vector=brp_f_vector,
                       brp_f_option=brp_f_option,

                       logf_sd=logf_sd,
                       f_dev_change=f_dev_change,
                       f_pattern=f_pattern,
                       start_val=start_val,
                       middle_val=middle_val,
                       end_val=end_val,
                       f_val=f_val,
                       start_year=start_year,
                       middle_year=middle_year,

                       logR_sd=logR_sd,
                       r_dev_change=r_dev_change,

                       om_bias_cor=om_bias_cor,
                       bias_cor_method=bias_cor_method,
                       em_bias_cor=em_bias_cor)
    edits <- list(...)
    base_stock <- `[<-`(base_stock, names(edits), edits)
    return(base_stock)
  } else {
    edits <- list(...)
    update_input <- `[<-`(input_list, names(edits), edits)
    return(update_input)
  }
}
