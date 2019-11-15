#' is.leapyear
#' check if a year is leap year
#' @param year
#' @export
is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(year[((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)])
}
