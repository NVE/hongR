#' readHBV_metinput
#' this function read the input met data of the HBV model
#' @param fileName
#' @export
readHBV_metinput <- function(fileName, skip = 0, header = TRUE, row.names = 1) {
	print(fileName)
	ObsRunoff <- read.table(fileName, header = TRUE, skip = skip, row.names = 1)
	ObsDates <- as.Date(rownames(ObsRunoff), format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff, ObsDates)

	return(ObsRunoffZoo)
}
