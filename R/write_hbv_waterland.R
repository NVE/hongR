#' write_hbv_waterland
#' this function writes a list object area into a file file_waterland
#' @param area
#' @param file_waterland
#' @export

write_hbv_waterland <- function(area = area, file_waterland = file) {
  file.create(file_waterland)
	for (iCat in length(area)) {
	  write.table(sprintf("# %d # %d", area[[iCat]]$id, length(area[[iCat]]$landindex)), file_waterland, quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
	  write.table(cbind(area[[iCat]]$landindex, area[[iCat]]$geoindex), file_waterland, quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
	}
}