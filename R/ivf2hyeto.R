#' calculate hyetograph from ivf
#' @param ivf
#' @param timestep # time in seconds
#' @param duration event total duration in seconds
#' @export
ivf2hyeto <- function(ivf = ivf, timestep = 10*60, duration =  60*60) {
  # the start ogf time event

ivf_table <- read.table(file = "C:\\Users\\honli\\Downloads\\ivf2hyeto.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
ivf_rowname <- as.numeric(unlist(strsplit(ivf_table[,1], " min", fixed = TRUE)))
ivf_rowname <- ivf_rowname[-which(is.na(ivf_rowname))]*60
ivf <- data.frame(value = ivf_table[,2], row.names = ivf_rowname)

  timestep <- 5*60
  duration <-  60*60
  row_names <- as.character(seq(0, duration, timestep))
  row_names <- row_names[-length(row_names)]

  n_length <- length(row_names)

  ivf2hyeto <- data.frame(value = rep(0, n_length), row.names = row_names)
  if (n_length %% 2 == 0) {
    i_n_start <- ceiling(n_length/2)
    for (i_n in seq(i_n_start, 1, -1)) {
      if (i_n == ceiling(n_length/2)) {
        ivf2hyeto$value[i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n+1])]
        ivf2hyeto$value[i_n+1] <- ivf$value[which(rownames(ivf) == row_names[i_n+1])]
      } else {

        ivf2hyeto$value[i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n])]-sum(ivf2hyeto$value)/2
        ivf2hyeto$value[n_length - i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n])]-sum(ivf2hyeto$value)/2

      }

    }

  } else {
    for (i_n in seq(ceiling(n_length/2), 1, -1)) {
      if (i_n == ceiling(n_length/2)) {
        ivf2hyeto$value[i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n])]
      } else {
        ivf2hyeto$value[i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n])]-sum(ivf2hyeto$value)/2
        ivf2hyeto$value[n_length - i_n] <- ivf$value[which(rownames(ivf) == row_names[i_n])]-sum(ivf2hyeto$value)/2

      }

    }
  }
  return(ivf2hyeto)
}
