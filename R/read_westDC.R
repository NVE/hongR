#' read_westDC
#' read westDC data store at the disk to a zoo object
#' @param met_path
#' @param years
#' @param time_step
#' @export
#' @example DataZoo <- read_westDC(met_path, years = c(), timestep = "daily", vars = c("rr", "tm"))
read_westDC <- function(met_path = NULL, years = NULL, vars = c("rr", "tm"), lon_lat = lon_lat) {
  #met_path <- "/data05/GlobalHydroPressure/data/APHRODITE/Xinjiang"
  require(R.utils)	 
  require(ncdf4) 
  met_dates <- seq(as.Date(sprintf("%d-01-01", years[1])), as.Date(sprintf("%d-12-31", years[length(years)])), by = "day")
  #met_dates <- seq(as.Date(sprintf("%d-01-01", years[1])), as.Date(sprintf("%d-01-01", years[length(years)]))) 

 #<- setNames(data.frame(matrix(ncol = 2*length(APHRODITE_index), nrow = length(met_dates)), rep(APHRODITE_index, 2))
  
    #time_index <- which(year(met_dates) == iY)

  for (iV in seq(length(vars))) { #"rr in mm/h"
      met_file <- sprintf("%s/%s.nc", met_path, vars[iV])
      met_id <- nc_open(met_file, write = FALSE, verbose = FALSE)
      met_data <- wrap(ncvar_get(met_id, vars[iV]), map = list(NA, 3))
      #met_time <- ncvar_get(nc_id, "time")

      #met_input[time_index, (1 + length(APHRODITE_index)*(iV - 1)):(length(APHRODITE_index)*iV)] <- met_data
      if (vars[iV] == "rr") {
         met_lon <- ncvar_get(met_id, "lon")
         met_lat <- ncvar_get(met_id, "lat")
         met_coord_cen <- NULL
         for (ilat in seq(length(met_lat))) {
                met_coord_cen <- rbind(met_coord_cen,
                                       cbind(met_lon,
                                             rep(met_lat[ilat], length(met_lon))))
         }
         colnames(met_coord_cen) <- c("lon", "lat")
	 met_index <- row.match(lon_lat, met_coord_cen)
         met_input_dfr <- met_data[met_index, ]*24 # "rr in mm/h"
      } else {
	 #print(met_data[met_index[1],])
	 met_data[met_index, ] <- met_data[met_index, ] - 273.15 # tmm in K
	 #print(met_data[met_index[1],])
 	 met_input_dfr <- rbind(met_input_dfr, met_data[met_index, ])
      }
      nc_close(met_id)	
  }

  APHRODITE <- zoo(t(met_input_dfr), met_dates)
  names_AP <- sprintf("lat%.3f_lon%.3f", lon_lat$lat, lon_lat$lon)
  names(APHRODITE) <- rep(names_AP, 2)	
  read_APHRODITE <- list(data = APHRODITE)
  return(read_APHRODITE)

}
