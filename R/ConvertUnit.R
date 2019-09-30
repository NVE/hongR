# convert unit
# area are in m2
ConvertUnit <- function(inN = 1, inUnit = "m3/s", outUnit = "mm/day", areaCat = 1)
{
  if ((inUnit == "m3/s") && (outUnit == "mm/day")) outN <- inN * 86400 * 1000 / areaCat
  if ((inUnit == "m3") && (outUnit == "mm")) outN <- inN * 1000 / areaCat
  if ((inUnit == "mm/day") && (outUnit == "m3/s")) outN <- inN * areaCat / (86400 * 1000)
  
  if ((inUnit == "kg.m-2.s-1") && (outUnit == "mm/day")) outN <- inN * 86400
  
  return (outN)
      
}