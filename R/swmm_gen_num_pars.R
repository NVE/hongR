#' swmm_gen_num_pars
#' this function generate parametersets based in the ranges
#' @param n
#' @param input a dataframe and one par for each column, row1: value row2: min row3: max
#' @export
swmm_gen_num_pars <- function(n=n, input = input) {
  value_row <- which(rownames(input) == "value")
  min_row <- which(rownames(input) == "min")
  max_row <- which(rownames(input) == "max")
  difference_row <- which(rownames(input) == "difference")
  num_par <-  matrix(nrow = n, ncol = dim(input)[2])
  for (iPar in seq(dim(input)[2])) {
    change_facor <- runif(n, min=as.numeric(input[min_row, iPar]), max=as.numeric(input[max_row, iPar])) 
    if (input[difference_row, iPar] == "scale") {
      num_par [, iPar] <- as.numeric(input[value_row, iPar]) * change_facor 
    } else if (input[difference_row, iPar] == "absolute") {
      num_par[, iPar] <- change_facor
    }
  
  }
  colnames(num_par) <- colnames(input)
  return(num_par) 
  
}