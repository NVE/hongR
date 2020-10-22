#' read_GRDC_discharge
#' this function read GRDC file
#' @param fileName
#' @export
read_GRDC_discharge <- function(fileName=fileName, time_step = "Month") {
	source("/data05/GlobalHydroPressure/code/HongR/isUnique.R")
	raw <- read.table(fileName, sep = ";", header = TRUE)
        if (isUnique(raw$YYYY.MM.DD) == FALSE) stop("date is not unique")
	if (time_step == "Month") {
		data_zoo <- zoo(raw$Calculated, as.Date(raw$YYYY.MM.DD, format = "%Y-%m-%d"))
	} else if (time_step == "Day") {

		data_zoo <- zoo(raw$Value, as.POSIXlt(paste(raw$YYYY.MM.DD, "12:00:00", sep = " "), tz = "GMT"))
	} else {
		stop(sprintf("missing time_step: %s", time_step))
	}			
	return(data_zoo)

}
