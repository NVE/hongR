#' ConvertUnit
#' @param inN
#' @param inUnit
#' @param outUnit
#' @param areaCat in m2
#' @export
ConvertUnit <- function(inN = 1, inUnit = "m3/s", outUnit = "mm/day", areaCat = 1)
{
  if ((inUnit == "m3/s") && (outUnit == "mm/day")) outN <- inN * 86400 * 1000 / areaCat
  if ((inUnit == "m3/s") && (outUnit == "mm/5min")) outN <- inN * 300 * 1000 / areaCat
  if ((inUnit == "m3/s") && (outUnit == "mm/min")) outN <- inN * 60 * 1000 / areaCat
  if ((inUnit == "m3/s") && (outUnit == "mm/hour")) outN <- inN * 3600 * 1000 / areaCat
  if ((inUnit == "l/s") && (outUnit == "mm/5min")) outN <- inN * 300 / areaCat
  

  if ((inUnit == "m3") && (outUnit == "mm")) outN <- inN * 1000 / areaCat
  if ((inUnit == "mm/day") && (outUnit == "m3/s")) outN <- inN * areaCat / (86400 * 1000)
  if ((inUnit == "mm/5min") && (outUnit == "m3/s")) outN <- inN * areaCat / (300 * 1000)
  
  if ((inUnit == "kg.m-2.s-1") && (outUnit == "mm/day")) outN <- inN * 86400
  
  
  return (outN)
      
}