#' readHBV_metstation
#' this function write a dataframe met to a file fileMet
#' @param fileMet
#' @export
read_hbv_metstation <- function(fileMet = fileMet) {
	#Final <- data.frame(type, id, x_m, y_m, height_m, name)
	data_in <- read.table(fileMet, skip = 2, header = FALSE, sep = " ", dec = ".", stringsAsFactors = FALSE)
        colnames(data_in) <- c("type", "id", "x_m", "y_m", "height_m", "name")
        return(data_in)	
}
