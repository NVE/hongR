write_hbv_landscape <- function(landscape_file = landscape_file, landscape = landscape) {
	# landscape <- list(ncols = 10, nrows = 10, xllcorner = 100, yllcorner = 100, cellsize = 1, NODATA_value = -9999, numberofelements = 1000, landscape_info = data.frame())
  write.table(sprintf("ncols         %d", landscape$ncols), file = landscape_file, append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("nrows         %d", landscape$nrows), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("xllcorner     %d", landscape$xllcorner), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("yllcorner     %d", landscape$yllcorner), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("cellsize      %d", landscape$cellsize), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("NODATA_value  %d", landscape$NODATA_value), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(sprintf("# Number of landscape elements : %d", landscape$numberofelements), file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  write.table(landscape$landscape_info, file = landscape_file, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)

}