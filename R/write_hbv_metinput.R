#'
#' This function write the HBV format input met data.
#' @param DataZoo data to write, row names are time stamps 20171102/1200
#' @param fileName file to write.
#'
#' @keywords data
#' @export
#' @examples
#' write_hbv_metinput()

write_hbv_metinput <- function(DataZoo, fileName = "data/input_data.txt") {
	if ( ! require(zoo) )  { install.packages("zoo");        library(zoo) }
	print(fileName)
        rownames(DataZoo) <- format(index(DataZoo), "%Y%m%d/1200") 

	write.table(t(c("Time", names(DataZoo))),
	            file = fileName, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = " ")
	write.table(round(DataZoo, digits = 3),
	  file = fileName, col.names = TRUE, row.names = TRUE, append = TRUE, quote = FALSE, sep = " ")

}
