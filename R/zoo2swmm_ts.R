zoo2swmm_ts <- function(zoo = precipitation, file = "prec.txt") {
  time_str <-  format(time(zoo), "%m/%d/%Y %H:%M")
  value_str <- sprintf("%.2f", as.numeric(zoo))
  write_str <- data.frame(time = time_str, value = value_str)
  write.table(write_str, file, col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
}