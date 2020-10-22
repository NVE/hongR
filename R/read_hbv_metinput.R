#' readHBV_metinput
#' this function read the input met data of the HBV model
#' @param fileName
#' @export
read_hbv_metinput <- function(fileName, skip = 1, header = TRUE, row.names = 1) {
	require(zoo)
	print(fileName)
	ObsRunoff <- read.table(fileName, header = TRUE, skip = skip, row.names = row.names)
	ObsDates <- as.Date(rownames(ObsRunoff), format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff, ObsDates)

	return(ObsRunoffZoo)
}
