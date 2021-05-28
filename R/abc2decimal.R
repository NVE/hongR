#' abc2decimal
#' to convert letters to integer a -> 1, z <- 26 only for lower case
#' @param ABCstr
#' @export
abc2decimal <- function(ABCstr) {
  if (grepl("^[a-z]+$", ABCstr, perl=T)) {
    ten <- 0
    #if (ABCstr == "a") {
    #  return(1)
    #}

    str_len = nchar(ABCstr);
    for(i in seq(str_len, 1, -1)) {
      temp_char = substr(ABCstr, i, i)
      int = utf8ToInt(temp_char)
      ten = ten + (int-96) * 26**(str_len - i)
    }
    return(ten)

  } else {
    stop("should be only letters in lower case")

  }

}

