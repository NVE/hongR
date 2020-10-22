#' write_hbv_metstation
#' this function write a dataframe met to a file fileMet
#' @param met
#' @param fileMet
#' @export
write_hbv_metstation <- function(met = met, fileMet = fileMet) {
	#Final <- data.frame(type, id, x_m, y_m, height_m, name)
	n_P <- length(which(met$type == "P"))
	n_T <- length(which(met$type == "T"))
	write.table(sprintf("Number of precipitation stations :  %d", n_P), fileMet, quote = FALSE, col.names = FALSE, row.names = FALSE)
	write.table(sprintf("Number of temperature   stations :  %d", n_T), fileMet, quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
	write.table(met, fileMet, quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE, sep = " ")
}
