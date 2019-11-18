#' decimal2abc
#' to convert letters to integer 1 -> a, 26 -> z only for lower case
#' @param start_num
#' @param series
#' @export
#' @example temp_obsnme <- decimal2abc(n_obs, series)
decimal2abc <- function(start_num, series = series) {
    if ((start_num <=0) || ((start_num %% 1) != 0))  {
      stop("input must be ingeter larger than 0")
    }
    return_str <- character(length = length(series))

    for (i_s in seq(length(series))) {
      print(i_s)
      if (series[i_s] == -9999) {
        ABCstr <- "!dum!"
      } else {
        ten <- start_num + i_s - 1
        ABCstr <- ""
        #print(ten)
        #print(ABCstr)
        while (ten != 0 ) {
          x = ten %% 26
          if (x == 0) {
            temp_str <- "Z"
          } else {
            temp_str <- intToUtf8(64+x)
          }
          ABCstr <- paste(temp_str, ABCstr, sep = "")
          if (x == 0) {
            ten <- (ten - 26)/26
          } else {
            ten <- (ten - x)/26
          }
        }

      }
      return_str[i_s] <- tolower(ABCstr)
    }
    return(return_str)
}

