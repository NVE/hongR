#' read nve data
#'
#' This function read the HBV format data and return a zoo object.
#' @param fileName file to read.
#' @param na.rm if remove the null values, "NULL" not include, "NA" eller "any"; default is "NULL"
#' @keywords data
#' @export
#' @examples
#' readHBV(fileName = "data/dew_00400000.out", na = "NULL", skip = 0)

readRegnByge <- function(fileName, na = "NULL", skip = 2) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)
	raw <- read.table(fileName, header = FALSE, skip = skip)
	raw[which(raw[,3] < 0),3] <- NA
	#ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	raw_time <- as.POSIXct(paste(raw[,1], raw[,2]), tz = "GMT")
	duplicate_time <- which(duplicated(raw_time))
	if (length(duplicate_time) >= 1) stop(sprintf("duplicate time %s", raw_time[duplicate_time]))
	# implement how to deal with repeat
	raw_zoo <- zoo(raw[,3], raw_time)
	if (na == "NULL") {
	  removeNULL <- which(is.na(raw_zoo))
	  if (length(removeNULL) == 0) {
	    return(raw_zoo)
	  } else {	
	    raw_zoo <- raw_zoo[-removeNULL]
	  }
	} else if (na == "NA") {
	  raw_zoo <- raw_zoo
	} else {
	  raw_zoo[which(is.na(raw_zoo))] <- na
	}
	
	# implement missing summary there
	
	# implement summary
	
	return(raw_zoo)
}
