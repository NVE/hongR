# if there is missing data and use method to fill
#' missing_data
#' this function fill missing data by a interpolate function
#' @param data
#' @param fill
#' @export

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
#' fill_missing_based_index
#' this function make a zoo object
#' @param data
#' @param fill
#' @export

fill_missing_based_index <- function(index_data = index_data, in_data = in_data, fill = NA, n.local = 2) {

	out_data <- zoo(rep(NA, length(index_data)), index_data)
	out_data[which(index_data %in% time(in_data))] <- in_data
	if (is.na(fill) == TRUE) {
		return(out_data)
	} else if (fill = "local.linear") {

	}

}
