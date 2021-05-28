#' calculate hyetograph from ivf based on vav method from vann of avløpe report
#' @param ivf matrix rownames is return period, colnames are desinged duration
#' @param timestep # time in seconds
#' @param duration event total duration in seconds
#' @export
ivf2hyeto <- function(ivf = ivf, timestep = 10*60, duration =  60*60) {

  ivf_time <- as.numeric(rownames(ivf))

  ivf_time_used <-seq(timestep, max(ivf_time), timestep)
  ivf_time_used <- ivf_time_used[-which(ivf_time_used %in% ivf_time)]
  x_new <- log(ivf_time_used)
  ivf_value_used <- matrix(nrow = length(ivf_time_used), ncol = dim(ivf)[2])
  rownames(ivf_value_used) <- ivf_time_used
  colnames(ivf_value_used) <- colnames(ivf)
  for (i_tr in seq(dim(ivf)[2])) {
    for (i_td in seq(length(ivf_time_used))) {
      max_lt <- max(ivf_time[which(ivf_time < ivf_time_used[i_td])])
      min_gt <- min(ivf_time[which(ivf_time > ivf_time_used[i_td])])

      x <- log(c(max_lt, min_gt))
      y <- log(ivf[c(which(ivf_time == max_lt), which(ivf_time == min_gt)), i_tr])
      ivf_lm <- lm(formula = y ~ x)
      ivf_int <- log(ivf_time_used[i_td]) * ivf_lm$coefficients[2] + ivf_lm$coefficients[1]
      ivf_int <- exp(ivf_int)
      ivf_value_used[i_td, i_tr] <- ivf_int
    }
  }

  ivf_new <- rbind(ivf, ivf_value_used)
  ivf_new <- ivf_new[order(as.numeric(rownames(ivf_new))), ]
  ivf_new_time <- as.numeric(rownames(ivf_new))

  ivf <- ivf_new
  ivf_time <- ivf_new_time

  ivf2hyeto_time <- seq(0, duration-timestep, timestep)
  n_length <- length(ivf2hyeto_time)

  ivf2hyeto <- data.frame(matrix(0, ncol = dim(ivf)[2], nrow = n_length), row.names = ivf2hyeto_time)
  colnames(ivf2hyeto) <- colnames(ivf)

  for (col_index in seq(dim(ivf2hyeto)[2])) {
    if (n_length %% 2 == 0) {
    # method 2 halv of the two time step
      i_n_start <- ceiling(n_length/2)
      for (i_n in seq(i_n_start, 1, -1)) {
        if (i_n == ceiling(n_length/2)) {
          ivf2hyeto[i_n, col_index] <- 0.5*ivf[which(ivf_time == 2*timestep), col_index]
          ivf2hyeto[i_n+1, col_index] <- ivf2hyeto[i_n, col_index]
        } else {
          total_time <- (i_n_start - i_n+1)*timestep*2
          ivf2hyeto[i_n, col_index] <- 0.5*(ivf[which(ivf_time == total_time), col_index]-sum(ivf2hyeto[,col_index]))
          ivf2hyeto[n_length - i_n + 1, col_index] <- ivf2hyeto[i_n, col_index]
        }
      }
    } else {
      i_n_start <- ceiling(n_length/2)
      for (i_n in seq(i_n_start, 1, -1)) {
        if (i_n == ceiling(n_length/2)) {
          ivf2hyeto[i_n, col_index] <- ivf[which(ivf_time == timestep), col_index]
        } else {
          total_time <- (n_length - 2*i_n + 2)*timestep
          ivf2hyeto[i_n, col_index] <- 0.5*(ivf[which(ivf_time == total_time), col_index]-sum(ivf2hyeto[,col_index]))
          ivf2hyeto[n_length - i_n + 1, col_index] <- ivf2hyeto[i_n, col_index]
        }
      }
    }
  }
  return(ivf2hyeto)
}
