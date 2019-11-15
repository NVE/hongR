## get hbv_parameters from files dew_common
#' get_cal_common_par
#' this function get the calibrated parameters based on comparsion of pest file and parameter file
#' @param tpl_file
#' @param par_file
#' @export
get_cal_common_par <- function(tpl_file = "dew_common_parameters.tpl", par_file = "dew_common_parameters.txt") {
	tpl <- scan(tpl_file, skip = 1, sep = ":", what = "character")
	tpl <- matrix(tpl, ncol = 2, byrow = TRUE)
	tpl <- tpl[,2]
	tpl_numeric <- as.numeric(tpl)
	par_loc <- which(is.na(tpl_numeric))
	par_name <- tpl[par_loc]
    par_name <- gsub(" ", "", par_name)
    par_name <- gsub("#", "", par_name)

	par <- scan(par_file, sep = ":", what = "character")
	par <- matrix(par, ncol = 2, byrow = TRUE)
	par <- par[,2]
	par_numeric <- as.numeric(par)
	par_values <- par_numeric[par_loc]
	re <- data.frame(name = par_name, value = par_values)
	return(re)
}
#' get_cal_land_par
#' this function get the calibrated parameters based on comparsion of pest file and parameter file
#' @param tpl_file
#' @param par_file
#' @export

get_cal_land_par <- function(tpl_file = "dew_landsurface_parameters.tpl", par_file = "dew_landsurface_parameters.txt") {
	tpl <- readLines(tpl_file, n = -1)
	tpl <- tpl[-c(1,2)]
	nLand <- length(tpl)

	par <- readLines(par_file, n = -1)
	par <- par[-1]

	loc_list <- str_locate_all(pattern = "#", tpl)
	par_name <- NULL
	par_values <- NULL
	for (iL in seq(length(loc_list))) {
		temp_loc <- matrix(loc_list[[1]][,1], ncol = 2, byrow = TRUE)
		for (iT in seq(dim(temp_loc)[1])) {
			par_name <- c(par_name, substr(tpl[iL], temp_loc[iT,1] + 1, temp_loc[iT,2] - 1))
           	par_values <- c(par_values, as.numeric(substr(par[iL], temp_loc[iT,1], temp_loc[iT,2])))
		}
	}
	par_name <- gsub(" ", "", par_name)

	re <- data.frame(name = par_name, value = par_values)
	re <- unique(re)

	return(re)
}
#' get_cal_soil_par
#' this function get the calibrated parameters based on comparsion of pest file and parameter file
#' @param tpl_file
#' @param par_file
#' @export
get_cal_soil_par <- function(tpl_file = "hbv_soil_parameters.tpl", par_file = "hbv_soil_parameters.txt") {
	tpl <- readLines(tpl_file, n = -1)
	tpl <- tpl[-c(1,2)]
	nLand <- length(tpl)

	par <- readLines(par_file, n = -1)
	par <- par[-1]

	loc_list <- str_locate_all(pattern = "#", tpl)
	par_name <- NULL
	par_values <- NULL
	for (iL in seq(length(loc_list))) {
		temp_loc <- matrix(loc_list[[1]][,1], ncol = 2, byrow = TRUE)
		for (iT in seq(dim(temp_loc)[1])) {
			par_name <- c(par_name, substr(tpl[iL], temp_loc[iT,1] + 1, temp_loc[iT,2] - 1))
           	par_values <- c(par_values, as.numeric(substr(par[iL], temp_loc[iT,1], temp_loc[iT,2])))
		}
	}
	par_name <- gsub(" ", "", par_name)

	re <- data.frame(name = par_name, value = par_values)
	re <- unique(re)

	return(re)
}
