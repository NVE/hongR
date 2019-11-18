#' swmm_out2ascii
#' this function writes the swmm output file fileIn into a ascii file fileOut
#' @param fileIn
#' @param fileOut
#' @param iType
#' @param object_name
#' @param vIndex
#' @param start_time_str
#' @param end_time_str
#' @export

swmm_out2ascii <- function(fileIn, fileOut, iType, object_name, vIndex, start_time_str, end_time_str) {
  #fileIn <- "C:\\Users\\honli\\Desktop\\DuringChina\\CatchmentDelineation\\swmm_runs\\pest_calib\\simplest.out"
  #fileOut <- "C:\\Users\\honli\\Desktop\\DuringChina\\CatchmentDelineation\\swmm_runs\\pest_calib\\simplest.txt"
  #iType <- 1
  #object_name <- "161143"
  #vIndex <- 4
  data_out <- read_out(file = fileIn, iType = iType, object_name = object_name, vIndex = vIndex)[[1]][[1]]

  start_time <- as.POSIXct(start_time_str, format = "%Y%m%dT%H%M%S", origin = "1970-01-01")
  end_time <- as.POSIXct(end_time_str, format = "%Y%m%dT%H%M%S", origin = "1970-01-01")

  data_out <- data_out[which(time(data_out) >= start_time & time(data_out) <= end_time)]

  time_str <- format(time(data_out), "%m/%d/%Y %H:%M")
  value_str <- sprintf("%f10", data_out)
  data_frame <- data.frame(time = time_str, value = value_str)

  write.table(data_frame, file = fileOut, row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

}

#write.zoo(test, file = fileOut)
