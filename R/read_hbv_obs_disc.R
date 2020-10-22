#' read nve data
#'
#' This function read the HBV format data and return a zoo object.
#' @param fileName file to read.
#' @param na.rm if remove the null values, "NULL" not include, "NA" eller "any"; default is "NULL"
#' @keywords data
#' @export
#' @examples

read_hbv_obs_disc <- function(fileName, na = "NULL", skip = 0) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)
	#obs_disc_head = gsub("#", "", readLines(fileName, n = 1))

	ObsRunoff <- read.table(fileName, header = FALSE, skip = skip)
	ObsRunoff[which(ObsRunoff[,2] == -9999),2] <- NA
	ObsDates <- as.POSIXct(ObsRunoff[,1], format = "%Y%m%d/%H%M", tz = "GMT")
	ObsRunoffZoo <- zoo(ObsRunoff[,2], ObsDates)
	if (na == "NULL") {
	  removeNULL <- which(is.na(ObsRunoffZoo))
	  if (length(removeNULL) == 0) {
	    return(ObsRunoffZoo)
	  } else {	
		ObsRunoffZoo <- ObsRunoffZoo[-removeNULL]
	  }
	} else if (na == "NA") {
		ObsRunoffZoo <- ObsRunoffZoo
	} else {
		ObsRunoffZoo[which(is.na(ObsRunoffZoo))] <- na
	}
	#ObsRunoff <- list(head = obs_disc_head, Runoff = ObsRunoffZoo)
	return(ObsRunoffZoo)
}
