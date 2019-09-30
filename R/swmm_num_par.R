# swmm_par
swmm_num_par <- function(type = "data.frame", inf_m = "GREEN_AMPT") {

  ## subcatchment
  ## subarea
  ## infiltration
  ## aquifer parameter
  ## groundwater parameter
  ## junctions
  ## conduits

  if (type == "data.frame" & inf_m == "GREEN_AMPT") {
    par <- data.frame(subcat_area  = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.9, 1.1, "scale", "state", "subcatchment area", "ha"),
                      subcat_imper = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "percentage of impervious area", "%"),
                      subcat_width = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "width", "m"),
                      subcat_slope = c(1, "subcatchment", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "mean or median slope", "%"),
                      subcat_curb  = c(1, "subcatchment", "gis", "polution", "Yes", 0.7, 1.3, "scale", "state", "curb", "-"),

                      subarea_mann_imper = c(0.001, "subarea", "gis", "hydrology", "Yes", 0.010, 0.033, "absolute", "state", "manning value for impervious area", "-"),
                      subarea_mann_per = c(0.01, "subarea", "gis", "hydrology", "Yes", 0.02, 0.8, "absolute", "state", "manning value for pervious area", "-"),
                      subarea_stora_imper = c(0.078, "subarea", "gis", "hydrology", "Yes", 0.3, 2.5, "absolute", "state", "storage for impervious area", "mm"),
                      subarea_stora_per = c(0.081, "subarea", "gis", "hydrology", "Yes", 0.5, 10, "absolute", "state", "storage for pervious area", "mm"),
                      subarea_stora_zero = c(100, "subarea", "gis", "hydrology", "Yes", 0, 100, "absolute", "state", "percentage of zero storage for impervious area", "-"),
                      subarea_PctRoute2P = c(100, "subarea", "gis", "hydrology", "Yes", 5, 100, "absolute", "state", "percentage to route within a subcatchment, subarea routing", "%"),

                      inf_G_S=c(100, "infiltration", "expert", "hydrology", "Yes", 10, 200, "absolute", "state", "average value of soil capalarity suction along the wetting front", "mm"),
                      inf_G_K=c(80, "infiltration", "expert", "hydrology", "Yes", 1, 200, "absolute", "state", "soil saturated hydraulic conductivity", "mm/hour"),
                      inf_G_I=c(0.15, "infiltration", "expert", "hydrology", "Yes", 0, 1, "absolute", "intial", "fraction of soil volume intial dry", "-"),

                      aq_Por=c(0.5, "aquifer", "expert", "hydrology", "Yes", 0.3, 0.7, "absolute", "state", "volume porosity", "-"),
                      aq_WP=c(0.15, "aquifer", "expert", "hydrology", "Yes", 0.047, 0.25, "scale", "state", "wilting point soil moisture content where plants cannot extract water from the soil relativ to Porsity and field capacity", "-"),
                      aq_FC=c(0.3, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.9, "scale", "state", "field capacity relativ to porosity", "-"),
                      aq_Ksat=c(10.0, "aquifer", "expert", "hydrology", "Yes", 1, 30, "absolute", "state", "saturated conductivity of soil in quifer", "mm/hour"),
                      aq_Kslope=c(10.0, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of conductivity vs. soil moisture deficit curve", "-"),
                      aq_Tslope=c(10, "aquifer", "expert", "hydrology", "Yes", 3, 44, "absolute", "state", "average slope of soil tension vs. soil moisture content curve", "-"),
                      aq_ETu=c(0.35, "aquifer", "expert", "hydrology", "Yes", 0.1, 0.5, "absolute", "state", "fraction of total eva avaliable eva in the upper zone", "-"),
                      aq_ETs=c(14, "aquifer", "expert", "hydrology", "Yes", 2, 14, "absolute", "state", "maximum depth into the lower saturated zone over which eva can occur", "m"),
                      aq_Seep=c(0.002, "aquifer", "expert", "hydrology", "Yes", 0.001, 0.005, "absolute", "state", "seepage rate from saturated zone to deep groundwater when water table is at the ground syrface", "mm/hour"),

                      ground_Esurf = c(1, "groundwater", "gis", "hydrology", "Yes", 0.7, 1.3, "scale", "state", "surface elevation of subcatchment relate to the subcatchmnet manhole ground elevation", "m"),
                      ground_A1=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-"),
                      ground_B1=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-"),
                      ground_A2=c(0.02, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-"),
                      ground_B2=c(1.25, "groundwater", "expert", "hydrology", "Yes", 1, 5, "absolute", "state", "groundwater coefficient", "-"),
                      ground_A3=c(0.01, "groundwater", "expert", "hydrology", "Yes", 0, 0.1, "absolute", "state", "groundwater coefficient", "-"),
                      ground_Ebot=c(0.5, "groundwater", "expert", "hydrology", "Yes", 0, 1, "scale", "state", "elevation of the bottom of the aquifer relative to the intial water table and surface elevation", "m"),
                      ground_Egw=c(0.2, "groundwater", "expert", "hydrology", "Yes", 0, 1, "scale", "intial", "groundwater table elevation at the start of simulation relative to the surface elevation", "-"),
                      ground_Umc=c(0.3, "groundwater", "expert", "hydrology", "Yes", 0.1, 1, "absolute", "intial", "unsaturated zone moisture content at start of simulation", "-"),

                      junctions_Elevation = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", " relativ to elevation of junction invert", "-"),
                      junctions_MaxDepth = c(1, "junctions", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "relative to depth from ground to invert elevation", "-"),
                      junctions_InitDepth = c(0.01, "junctions", "gis", "hydraulic", "yes", 0, 1, "scale", "intial", "relative to max depth", "-"),

                      conduits_Length = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1, "scale", "state", "length relative to GIS source", "-"),
                      conduits_Roughness = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "roughness relative to GIS source", "-"),
                      conduits_InOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "in elevation relativ to GIS source", "-"),
                      conduits_OutOffset = c(1, "conduits", "gis", "hydraulic", "yes", 0.7, 1.3, "scale", "state", "out elevation relativ to GIS source", "-"),

                      conduits_InitFlow = c(0, "conduits", "gis", "hydraulic", "yes", 0, 10, "absolute", "intial", "flow at the start of simulation", "flow unit")
    )

  } else if (type == "data.frame" & inf_m == "HORTON") {

    par <- data.frame(subcat_area  = c(1, "subcatchment", "gis", "hydrology", "Yes", 0, NA),
                      subcat_imper = c(1, "subcatchment", "gis", "hydrology", "Yes", 0, NA),
                      subcat_width = c(1, "subcatchment", "gis", "hydrology", "Yes", 0, NA),
                      subcat_slope = c(1, "subcatchment", "gis", "hydrology", "Yes", 0, NA),
                      subcat_curb  = c(1, "subcatchment", "gis", "polution", "Yes", 0, NA),

                      subarea_mann_imper = c(0.01, "subarea", "gis", "hydrology", "Yes"),
                      subarea_mann_per = c(0.5, "subarea", "gis", "hydrology", "Yes"),
                      subarea_stora_imper = c(0, "subarea", "gis", "hydrology", "Yes"),
                      subarea_stora_per = c(0, "subarea", "gis", "hydrology", "Yes"),
                      subarea_PctRoute2P = c(0, "subarea", "gis", "hydrology", "Yes"),

                      inf_MaxRate = c(4.5, "infiltration", "expert", "hydrology", "Yes"),
                      inf_MinRate = c(0.2, "infiltration", "expert", "hydrology", "Yes"),
                      inf_Decay = c(6.5, "infiltration", "expert", "hydrology", "Yes"),
                      inf_DryTime = c(7, "infiltration", "expert", "hydrology", "Yes"),
                      inf_MaxInfil = c(0, "infiltration", "expert", "hydrology", "Yes"),

                      aq_Por=c(0.5, "aquifer", "expert", "hydrology", "Yes"),
                      aq_WP=c(0.15, "aquifer", "expert", "hydrology", "Yes"),
                      aq_FC=c(0.3, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Ksat=c(10.0, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Kslope=c(10.0, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Tslope=c(10, "aquifer", "expert", "hydrology", "Yes"),
                      aq_ETu=c(0.35, "aquifer", "expert", "hydrology", "Yes"),
                      aq_ETs=c(14, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Seep=c(0.002, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Ebot=c(0, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Egw=c(10, "aquifer", "expert", "hydrology", "Yes"),
                      aq_Umc=c(0.3, "aquifer", "expert", "hydrology", "Yes"),

                      ground_A1=c(0.02, "groundwater", "expert", "hydrology", "Yes"),
                      ground_B1=c(1.25, "groundwater", "expert", "hydrology", "Yes"),
                      ground_A2=c(0.02, "groundwater", "expert", "hydrology", "Yes"),
                      ground_B2=c(1.25, "groundwater", "expert", "hydrology", "Yes"),
                      ground_A3=c(0, "groundwater", "expert", "hydrology", "Yes"),
                      ground_Dsw=c(0, "groundwater", "expert", "hydrology", "Yes"),
                      ground_Egwt=c(0, "groundwater", "expert", "hydrology", "Yes"),
                      ground_Wgr = c(0, "groundwater", "expert", "hydrology", "Yes"),
                      ground_Umc = c(0, "groundwater", "expert", "hydrology", "Yes"),

                      junctions_Elevation = c(1, "junctions", "gis", "hydraulic", "yes", 0, NA),
                      junctions_MaxDepth = c(1, "junctions", "gis", "hydraulic", "yes", 0, NA),
                      junctions_InitDepth = c(0, "junctions", "gis", "hydraulic", "yes", 0, NA),
                      junctions_SurDepth = c(0, "junctions", "gis", "hydraulic", "yes", 0, NA),

                      conduits_Length = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA),
                      conduits_Roughness = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA),
                      conduits_InOffset = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA),
                      conduits_OutOffset = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA),
                      conduits_InitFlow = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA),
                      conduits_MaxFlow = c(0, "conduits", "gis", "hydraulic", "yes", 0, NA)
    )

  } else {
    stop("need type and infiltration method")
  }
  rownames(par) <- c("value", "group", "source", "process", "calibratable", "min", "max", "difference", "intial", "meaning", "unit")

  return(par)
}
