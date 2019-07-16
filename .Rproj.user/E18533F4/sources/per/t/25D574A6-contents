#' ConvertUnit
#' @param inNumber
#' @param inUnit
#' @param outUnit
#' @param areaCat catchment area in case needed for example runoff to discharge
#' @author lihong2291@gmail.com
ConvertUnit <- function(inNumber = 1, inUnit = "m3/s", outUnit = "mm/day", areaCat = 1, areaUnit = "km2")
{
  if ((inUnit == "m3/s") && (outUnit == "mm/day")) outN <- inNumber * 86400 * 1000 / areaCat
  if ((inUnit == "m3") && (outUnit == "mm")) outN <- inNumber * 1000 / areaCat
  if ((inUnit == "mm/day") && (outUnit == "m3/s")) outN <- inNumber * areaCat / (86400 * 1000)

  if ((inUnit == "kg.m-2.s-1") && (outUnit == "mm/day")) outN <- inNumber * 86400

  return (outN)

}
