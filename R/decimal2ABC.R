#' decimal2abc
#' to convert letters to integer 1 -> a, 26 -> z only for lower case
#' @param num
#' @export
decimal2abc <- function(num) {
    if ((num <=0) || ((num %% 1) != 0))  {
      stop("input must be ingeter larger than 0")
    }
    ABCstr <- ""
    ten <- num

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

    return(tolower(ABCstr))

}

