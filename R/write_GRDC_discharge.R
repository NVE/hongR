#' write_GRDC_discharge
#' this function write GRDC file
#' @param fileName
#' @param DataZoo
#' @param TimeStep
#' @export
write_GRDC_discharge <- function(fileName=fileName, DataZoo = DataZoo, TimeStep = "Month") {
	library(zoo)
	data_length <- length(time(DataZoo))
	if (TimeStep == "Day") {
		dfr <- data.frame(Date = format(time(DataZoo), "%Y-%m-%d"), 
				  Time = rep("--:--", data_length),
				  Value = round(as.numeric(DataZoo), 3))
		colnames(dfr) <- c("YYYY-MM-DD", "hh:mm", "Value")
	} else {
               dfr <- data.frame(Date = format(time(DataZoo), "%Y-%m-%d"),
                                 Time = rep("--:--", data_length),
                                 Original = round(as.numeric(DataZoo$Original), 3),
				 Calculated = round(as.numeric(DataZoo$Calculated), 3),
				 Flag = round(as.numeric(DataZoo$Flag), 0))
                colnames(dfr) <- c("YYYY-MM-DD", "hh:mm", "Original", "Calculated", "Flag")	

	}		
	write.table(dfr, file = fileName, col.names = TRUE, row.names = FALSE, eol = "\n", quote = FALSE, dec = ".", sep = ";") 
}
