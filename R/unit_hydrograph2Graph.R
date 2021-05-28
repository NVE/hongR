UH2H <- function(R, t_big, K, delta_t, P) {
# https://swmm5.org/2016/09/04/rainfall-dependent-inflow-and-infiltration-from-the-epa-swmm-5-hydrology-manual/
  Q_return <- NULL
  for (t in seq(1, 100)) {
    Q_t <- 0
    for (j in seq(1, t)) {
      print(Q_t)
      #delta_t_j <- (j-0.5)*delta_t
      #delta_t_j_T <- (j-T-0.5)*delta_t

      delta_t_j <- j*delta_t
      delta_t_j_T <- (j-t_big)*delta_t
      
      if (delta_t_j <= t_big) f_j <- delta_t_j/t_big
      if ((t_big <= delta_t_j) & (delta_t_j <= t_big+K*t_big)) f_j <- 1 - delta_t_j_T/(K*t_big)
      if (delta_t_j > t_big+K*t_big) f_j <- 0
      
      U_j = 2*R*f_j/(t_big+K*t_big)
      
      if (j > length(P)) {
        P_input <- 0
      } else {
        P_input <- P[j]
      }
      Q_t <- Q_t + U_j*P_input
    }
    
    Q_return <- c(Q_return, Q_t)
  }
	return(Q_return)
  
}