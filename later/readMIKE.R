#' read mike ascii file, sep = ";", dec = "."
#'
#' This function read the HBV format data and return a zoo object.
#' @param fileName file to read.
#' @param na.rm if remove the null values, "NULL" not include, "NA" eller "any"; default is "NULL"
#' @keywords data
#' @export
#' @examples
#' readHBV(fileName = "data/dew_00400000.out", na = "NULL", skip = 0)

readMIKE <- function(fileName, na = "NULL", skip = 2, dec = ",", sep = ";") {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)
	raw <- read.table(fileName, header = FALSE, skip = skip, sep = sep, dec = dec)
	#ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	raw_time <- as.POSIXct(raw[,1])
	
	raw_zoo <- zoo(raw[,2:dim(raw)[2]], raw_time)
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
	return(raw_zoo)
}
