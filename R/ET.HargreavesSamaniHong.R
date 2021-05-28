#' calculate ET based on HargreavesSamani
#' @param data
#' @param constants
#' @export
ET.HargreavesSamaniHong <- function (data, constants)
{
    Ta <- (data$Tmax + data$Tmin)/2
    P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
    delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta +
        237.3)^2)
    gamma <- 0.00163 * P/constants$lambda
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
    delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
    w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
    N <- 24/pi * w_s
    R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
        sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
        sin(w_s))
	C_HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax - data$Tmin) + 0.4023

    DailyPE <- 0.0135 * C_HS * R_a/constants$lambda * (data$Tmax - data$Tmin)^0.5 * (Ta + 17.8)
	return(DailyPE)
}

