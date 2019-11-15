#' swmm_num_par_pest
#' this function
#' @param type
#' @param inf_m
#' @export

swmm_num_par_pest <- function(type = "data.frame", inf_m = "GREEN_AMPT") {

  ## subcatchment
  ## subarea
  ## infiltration
  ## aquifer parameter
  ## groundwater parameter
  ## junctions
  ## conduits

  if (type == "data.frame" & inf_m == "GREEN_AMPT") {
    par <- data.frame(area  = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.9, 1.1, "scale", "state", "subcatchment area", "ha", "no", ""),
                      imper = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "percentage of impervious area", "%", "no", "imper"),
                      width = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "width", "m", "no", "width"),
                      slope = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "mean or median slope", "%", "no", "slope"),
                      curb  = c(1, "subcatchment", "gis", "polution", "Yes", 0.7, 1.3, "scale", "state", "curb", "-", "no", ""),

                      mann_imper = c(0.01, "subarea", "gis", "hydrology", "Yes", 0.01, 0.015, "absolute", "state", "manning value for impervious area", "-", "yes", "mann_imper"),
                      mann_per = c(0.2,    "subarea", "gis", "hydrology", "Yes",    0.2,  0.3, "absolute", "state", "manning value for pervious area", "-", "yes", "mann_per"),
                      stora_imper = c(1.5, "subarea", "gis", "hydrology", "Yes", 0.5, 3.5, "absolute", "state", "storage for impervious area", "mm", "yes", "stora_imper"),
                      stora_per = c(5.0,   "subarea", "gis", "hydrology", "Yes", 4.0, 6, "absolute", "state", "storage for pervious area", "mm", "yes", "stora_per"),
                      stora_zero = c(20,   "subarea", "gis", "hydrology", "Yes", 1, 100, "absolute", "state", "percentage of zero storage for impervious area", "-", "yes", "stora_zero"),
                      PctRoute2P = c(30,   "subarea", "gis", "hydrology", "Yes", 15, 60, "absolute", "state", "percentage to route within a subcatchment, subarea routing", "%", "yes", "PctRoute2P"),

                      G_S=c(100, "infiltration", "expert", "hydrology", "Yes", 10, 200, "absolute", "state", "average value of soil capalarity suction along the wetting front", "mm", "yes", "G_S"),
                      G_K=c(80, "infiltration", "expert", "hydrology", "Yes", 50, 120, "absolute", "state", "soil saturated hydraulic conductivity", "mm/hour", "yes", "G_K"),
                      G_I=c(0.15, "infiltration", "expert", "hydrology", "Yes", 0.001, 0.9, "absolute", "intial", "fraction of soil volume intial dry", "-", "yes", "G_I"),

                      Por=c(0.5, "aquifer", "expert", "hydrology", "Yes", 0.4, 0.6,    "absolute", "state", "volume porosity", "-", "yes", "Por"),
                      WP=c(0.15, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.2,    "scale", "state", "wilting point soil moisture content where plants cannot extract water from the soil relativ to Porsity and field capacity", "-", "yes", "WP"),
                      FC=c(0.3, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.5, "scale", "state", "field capacity relativ to porosity", "-", "yes", "FC"),
                      Ksat=c(10.0, "aquifer", "expert", "hydrology", "Yes", 5, 30, "absolute", "state", "saturated conductivity of soil in quifer", "mm/hour", "yes", "Ksat"),
                      Kslope=c(10, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of conductivity vs. soil moisture deficit curve", "-", "yes", "KSlope"),
                      Tslope=c(10, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of soil tension vs. soil moisture content curve", "-", "yes", "Tslope"),
                      ETu=c(0.35, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.5, "absolute", "state", "fraction of total eva avaliable eva in the upper zone", "-", "yes", "ETu"),
                      ETs=c(14, "aquifer", "expert", "hydrology", "Yes", 5, 25, "absolute", "state", "maximum depth into the lower saturated zone over which eva can occur", "m", "yes", "ETs"),
                      Seep=c(0.002, "aquifer", "expert", "hydrology", "Yes", 0.001, 0.005, "absolute", "state", "seepage rate from saturated zone to deep groundwater when water table is at the ground syrface", "mm/hour", "yes", "Seep"),

                      Esurf = c(1, "groundwater", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "surface elevation of subcatchment relate to the subcatchmnet manhole ground elevation", "m", "no", "Esurf"),
                      A1=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0.0001, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "A1"),
                      B1=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-", "yes", "B1"),
                      A2=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0.0001, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "A2"),
                      B2=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-", "yes", "B2"),
                      A3=c(0.01, "groundwater", "expert", "hydrology", "Yes", 0.0001, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "A3"),
                      Ebot=c(0.5, "groundwater", "expert", "hydrology", "Yes", 0.0001, 1, "scale", "state", "elevation of the bottom of the aquifer relative to the intial water table and surface elevation", "m", "no", ""),
                      Egw=c(0.2, "groundwater", "expert", "hydrology", "Yes", 0.0001, 1, "scale", "intial", "groundwater table elevation at the start of simulation relative to the surface elevation", "-", "no", ""),
                      Umc=c(0.3, "groundwater", "expert", "hydrology", "Yes", 0.1, 1, "absolute", "intial", "unsaturated zone moisture content at start of simulation", "-", "no", ""),

                      Elevation = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", " relativ to elevation of junction invert", "-", "no", ""),
                      MaxDepth = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "relative to depth from ground to invert elevation", "-", "no", ""),
                      InitDepth = c(0.01, "junctions", "gis", "hydraulic", "yes", 0, 1, "scale", "intial", "relative to max depth", "-", "no", ""),

                      Length = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1, "scale", "state", "length relative to GIS source", "-", "no", ""),
                      Roughness = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "roughness relative to GIS source", "-", "no", "Roughness"),
                      InOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "in elevation relativ to GIS source", "-", "no", ""),
                      OutOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "out elevation relativ to GIS source", "-", "no", ""),

                      InitFlow = c(0.0002, "conduits", "gis", "hydraulic", "yes", 0.0001, 1, "absolute", "intial", "flow at the start of simulation", "-","yes", "InitFlow")
    )

  } else {
    stop("need type and infiltration method")
  }

  rownames(par) <- c("value", "group", "source", "process", "calibratable", "min", "max", "difference", "intial", "meaning", "unit", "pest", "pest_name")

  return(par)
}
