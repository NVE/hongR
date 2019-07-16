# if there is missing data and use method to fill

missing_data <- function(data = data, fill = NA) {
# input data is a zoo class
# fill is fill method, if it is na, only show missing time. "linear":linear interpolate
	missing_index <- which(diff(time(data))>1)

	if (is.na(fill) == TRUE) {
		missing_gaps <- data[unique(c(missing_index, missing_index + 1))]
		return(missing_gaps)
	} else if (fill == "linear") {
		return(data)
	} else {
		stop("error")
	}
}