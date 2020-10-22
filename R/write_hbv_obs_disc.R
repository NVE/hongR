#' writeHBV_obs_disc.R
#'
#' This function write the HBV format discharge data.
#' @param fileName file to write.

#' @keywords data
#' @export
#' @examples
#' writeHBV_disc(DataZoo, fileName =  "data/discharge_data.txt")

write_hbv_obs_disc <- function(DataZoo, fileName, head = "dew_0000000.out") {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	#print(fileName)
	if (is.null(dim(DataZoo)) == TRUE) {
		dim(DataZoo) <- c(length(DataZoo), 1)
	} 
 	rownames(DataZoo) <- format(index(DataZoo), "%Y%m%d/1200")
        write.table(sprintf("#%s", head), file = fileName, col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t")
	write.table(DataZoo, file = fileName, col.names = FALSE, row.names = TRUE, quote = FALSE, sep = "\t", append = TRUE)
}
