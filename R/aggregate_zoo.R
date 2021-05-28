#' aggregate accpring to n
#' @param in_zoo the input zoo
#' @param n


aggregate_zoo <- function(in_zoo = in_zoo, n = 2, na.rm = TRUE) {
  library(zoo)
  library(pracma)
  source("C:/Users/honli/Documents/hongR/R/movavg_Hong.R")
  #in_zoo <- sim_disch_stormwater
  out_data <- movavg_Hong(in_zoo, n = n, type = "s", na.rm = TRUE)[seq(n, length(in_zoo), n)]
  out_dates <- time(in_zoo)[seq(1, length(in_zoo), n)]
  out_zoo <- zoo(out_data, out_dates)
  return(out_zoo)

}
