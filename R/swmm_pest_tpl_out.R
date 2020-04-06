#' swmm_pest_tpl_out
#' make pest demiliter based on swmm_par
#' @param swmm_par
#' @param par_name
#' @export

swmm_pest_tpl_out <- function(swmm_par, par_name) {
  print(par_name)
  if (swmm_par["pest", par_name] == "yes") {
    print(par_name)
    swmm_pest_tpl_out <- sprintf("# %s #", swmm_par["pest_name", par_name])
  } else {
    print(par_name)
    swmm_pest_tpl_out <- round(as.numeric(swmm_par["value", par_name]), 20)
  }
}
