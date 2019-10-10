# swmm_par
swmm_num_par_pest <- function(type = "data.frame", inf_m = "GREEN_AMPT") {

  ## subcatchment
  ## subarea
  ## infiltration
  ## aquifer parameter
  ## groundwater parameter
  ## junctions
  ## conduits

  if (type == "data.frame" & inf_m == "GREEN_AMPT") {
    par <- data.frame(subcat_area  = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.9, 1.1, "scale", "state", "subcatchment area", "ha", "no", ""),
                      subcat_imper = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "percentage of impervious area", "%", "no", "imper"),
                      subcat_width = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "width", "m", "no", "width"),
                      subcat_slope = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "mean or median slope", "%", "no", "slope"),
                      subcat_curb  = c(1, "subcatchment", "gis", "polution", "Yes", 0.7, 1.3, "scale", "state", "curb", "-", "no", ""),

                      subarea_mann_imper = c(0.001, "subarea", "gis", "hydrology", "Yes", 0.010, 0.033, "absolute", "state", "manning value for impervious area", "-", "yes", "mann_imper"),
                      subarea_mann_per = c(0.01, "subarea", "gis", "hydrology", "Yes", 0.02, 0.8, "absolute", "state", "manning value for pervious area", "-", "yes", "mann_per"),
                      subarea_stora_imper = c(0.078, "subarea", "gis", "hydrology", "Yes", 0.3, 2.5, "absolute", "state", "storage for impervious area", "mm", "yes", "stor_imper"),
                      subarea_stora_per = c(0.081, "subarea", "gis", "hydrology", "Yes", 0.5, 10, "absolute", "state", "storage for pervious area", "mm", "yes", "stor_per"),
                      subarea_stora_zero = c(100, "subarea", "gis", "hydrology", "Yes", 0, 100, "absolute", "state", "percentage of zero storage for impervious area", "-", "yes", "stor_zero"),
                      subarea_PctRoute2P = c(20, "subarea", "gis", "hydrology", "Yes", 5, 100, "absolute", "state", "percentage to route within a subcatchment, subarea routing", "%", "yes", "PctRout"),

                      inf_G_S=c(100, "infiltration", "expert", "hydrology", "Yes", 10, 200, "absolute", "state", "average value of soil capalarity suction along the wetting front", "mm", "yes", "infGS"),
                      inf_G_K=c(80, "infiltration", "expert", "hydrology", "Yes", 1, 200, "absolute", "state", "soil saturated hydraulic conductivity", "mm/hour", "yes", "infGK"),
                      inf_G_I=c(0.15, "infiltration", "expert", "hydrology", "Yes", 0, 1, "absolute", "intial", "fraction of soil volume intial dry", "-", "yes", "infGI"),

                      aq_Por=c(0.5, "aquifer", "expert", "hydrology", "Yes", 0.3, 0.7, "absolute", "state", "volume porosity", "-", "yes", "aq_Por"),
                      aq_WP=c(0.15, "aquifer", "expert", "hydrology", "Yes", 0.047, 0.25, "scale", "state", "wilting point soil moisture content where plants cannot extract water from the soil relativ to Porsity and field capacity", "-", "yes", "aq_WP"),
                      aq_FC=c(0.3, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.9, "scale", "state", "field capacity relativ to porosity", "-", "yes", "aq_FC"),
                      aq_Ksat=c(10.0, "aquifer", "expert", "hydrology", "Yes", 1, 30, "absolute", "state", "saturated conductivity of soil in quifer", "mm/hour", "yes", "aq_Ksat"),
                      aq_Kslope=c(10.0, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of conductivity vs. soil moisture deficit curve", "-", "yes", "aq_KSlope"),
                      aq_Tslope=c(10, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of soil tension vs. soil moisture content curve", "-", "yes", "aq_Tslope"),
                      aq_ETu=c(0.35, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.5, "absolute", "state", "fraction of total eva avaliable eva in the upper zone", "-", "yes", "aq_ETu"),
                      aq_ETs=c(14, "aquifer", "expert", "hydrology", "Yes", 2, 14, "absolute", "state", "maximum depth into the lower saturated zone over which eva can occur", "m", "yes", "aq_ETs"),
                      aq_Seep=c(0.002, "aquifer", "expert", "hydrology", "Yes", 0.001, 0.005, "absolute", "state", "seepage rate from saturated zone to deep groundwater when water table is at the ground syrface", "mm/hour", "yes", "aq_Seep"),

                      ground_Esurf = c(1, "groundwater", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "surface elevation of subcatchment relate to the subcatchmnet manhole ground elevation", "m", "no", ""),
                      ground_A1=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "ground_A1"),
                      ground_B1=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-", "yes", "ground_B1"),
                      ground_A2=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "ground_A2"),
                      ground_B2=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-", "yes", "ground_B2"),
                      ground_A3=c(0.01, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-", "yes", "ground_A3"),
                      ground_Ebot=c(0.5, "groundwater", "expert", "hydrology", "Yes", 0, 1, "scale", "state", "elevation of the bottom of the aquifer relative to the intial water table and surface elevation", "m", "no", ""),
                      ground_Egw=c(0.2, "groundwater", "expert", "hydrology", "Yes", 0, 1, "scale", "intial", "groundwater table elevation at the start of simulation relative to the surface elevation", "-", "no", ""),
                      ground_Umc=c(0.3, "groundwater", "expert", "hydrology", "Yes", 0.1, 1, "absolute", "intial", "unsaturated zone moisture content at start of simulation", "-", "no", ""),

                      junctions_Elevation = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", " relativ to elevation of junction invert", "-", "no", ""),
                      junctions_MaxDepth = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "relative to depth from ground to invert elevation", "-", "no", ""),
                      junctions_InitDepth = c(0.01, "junctions", "gis", "hydraulic", "yes", 0, 1, "scale", "intial", "relative to max depth", "-", "no", ""),

                      conduits_Length = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1, "scale", "state", "length relative to GIS source", "-", "no", ""),
                      conduits_Roughness = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "roughness relative to GIS source", "-", "no", "cond_rough"),
                      conduits_InOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "in elevation relativ to GIS source", "-", "no", ""),
                      conduits_OutOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "out elevation relativ to GIS source", "-", "no", ""),

                      conduits_InitFlow = c(0, "conduits", "gis", "hydraulic", "yes", 0, 10, "absolute", "intial", "flow at the start of simulation", "-","yes", "cond_intf")
    )

  } else {
    stop("need type and infiltration method")
  }

  rownames(par) <- c("value", "group", "source", "process", "calibratable", "min", "max", "difference", "intial", "meaning", "unit", "pest", "pest_name")

  return(par)
}
