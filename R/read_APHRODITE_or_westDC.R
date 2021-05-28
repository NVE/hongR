#' read_APHRODITE
#' read APHRODITE data store at the disk to a zoo object
#' @param met_path
#' @param years
#' @param APHRODITE
#' @export
#' @example DataZoo <- read_APHRODITE(met_path, years = c(), timestep = "daily", vars = c("rr", "tm"), APHRODITE_index)
read_APHRODITE_or_westDC <- function(met_path = NULL, years = NULL, time_step = "day", vars = c("rr", "tm")) {
  #met_path <- "/data05/GlobalHydroPressure/data/APHRODITE/Xinjiang"
  require(R.utils)
  require(ncdf4)
  if (time_step == "Day") {	  
  	met_dates <- seq(as.Date(sprintf("%d-01-01", years[1])), as.Date(sprintf("%d-12-31", years[length(years)])), by = "day")
  } else if (time_step == "Month") {
	met_dates <- seq(as.Date(sprintf("%d-01-01", years[1])), as.Date(sprintf("%d-12-01", years[length(years)])), by = "month")
  }		
  #met_dates <- seq(as.Date(sprintf("%d-01-01", years[1])), as.Date(sprintf("%d-01-01", years[length(years)]))) 
  met_input_dfr <- NULL
 #<- setNames(data.frame(matrix(ncol = 2*length(APHRODITE_index), nrow = length(met_dates)), rep(APHRODITE_index, 2))
  
    #time_index <- which(year(met_dates) == iY)
  for (iV in seq(length(vars))) {
      met_file <- sprintf("%s/%s.nc", met_path, vars[iV])
      met_id <- nc_open(met_file, write = FALSE, verbose = FALSE)
      met_data <- ncvar_get(met_id, vars[iV])
      print(sprintf("dimension of %s is %d", vars[iV], dim(met_data)))
      if (iV == 1) {
         lon <- ncvar_get(met_id, "lon")
         lat <- ncvar_get(met_id, "lat")
      }	
      nc_close(met_id)
      met_input_temp <- wrap(met_data, map = list(NA, 3))
      met_input_dfr <- cbind(met_input_dfr, t(met_input_temp))
  }
  APHRODITE <- zoo(met_input_dfr, met_dates)
  lons <- NULL
  lats <- NULL
  names_AP <- NULL
  for (ilat in seq(length(lat))) {
        lats <- c(lats, rep(lat[ilat], length(lon)))
	lons <- c(lons, lon)
        for (ilon in seq(length(lon))) {
        	names_AP <- c(names_AP, sprintf("lat%.3f_lon%.3f", lat[ilat], lon[ilon]))
	}
  }
  names(APHRODITE) <- rep(names_AP, 2)	
  read_APHRODITE <- list(lon = lons, lat = lats, data = APHRODITE)
  return(read_APHRODITE)

}








