#' read swmm time series format data
#'
#' This function read the HBV format data and return a zoo object.
#' @param fileName file to read.
#' @param na.rm if remove the null values, "NULL" not include, "NA" eller "any"; default is "NULL"
#' @keywords data
#' @export
#' @examples

readSWMM_ts_old <- function(fileName, na = "NULL", skip = 0) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)

	raw <- read.table(fileName, header = FALSE, skip = skip, sep = "\t", dec = ".")
	raw_time <- as.POSIXct(as.character(raw[,1]), format = "%m/%d/%Y %H:%M", tz = "GMT", origin = "1970-01-01");

	duplicate_time <- which(duplicated(raw_time))
	if (length(duplicate_time) >= 1) stop(sprintf("duplicate time %s", raw_time[duplicate_time]))
	# implement how to deal with repeat
	raw_zoo <- zoo(raw[,2], raw_time)
	if (is.null(na) == TRUE) {
	  removeNULL <- which(is.na(raw_zoo))
	  if (length(removeNULL) == 0) {
	    return(raw_zoo)
	  } else {
	    raw_zoo <- raw_zoo[-removeNULL]
	  }
	} else if (is.na(na) == TRUE) {
	  raw_zoo <- raw_zoo
	} else {
	  raw_zoo[which(is.na(raw_zoo))] <- -9999
	}

	# implement missing summary there

	# implement summary

	return(raw_zoo)

}
