fill_missing_monthly <- function(InData) {
	library(hydroTSM)
	Montly <- monthlyfunction(InData, na.rm = TRUE, FUN = mean)
	Monthly_lab <- as.character(index(Montly))

	Missing_index <- which(is.na(InData))
	for (imissing in Missing_index) {
		InData[imissing] <- Montly[which(Monthly_lab == format(index(InData[imissing]), "%b"))]
	}	

	return(InData)
}
