# swmm_par
swmm_default_par <- function(type = "list", inf_m = "GREEN_AMPT") {
  
  Mann_n_imper <- 0.01
  Mann_n_per <- 0.5
  S_Imper <- 0
  S_Per <- 0
  Route2P <- "IMPERVIOUS"
  PctRoute2P <- 0
  
  ## infiltration parameter for GREEN-AMPT
  inf_G_S <- 100
  inf_G_K <- 10
  inf_G_I <- 0.00115

  ## infiltration parameter for HORTON
  inf_MaxRate <- 4.5 
  inf_MinRate <- 0.2
  inf_Decay <- 6.5
  inf_DryTime <- 7
  inf_MaxInfil <- 0
  
  
  
  ## aquifer parameter
  aq_Name <- "aquifers_1"
  aq_Por <- 0.5
  aq_WP <- 0.15
  aq_FC <- 0.3
  aq_Ksat <- 10.0
  aq_Kslope <- 10.0
  aq_Tslope <- 10 
  aq_ETu <- 0.35
  aq_ETs <- 14
  aq_Seep <- 0.002
  aq_Ebot <- 0
  aq_Egw <- 10
  aq_Umc <- 0.3
  aq_ETupat <- NA
  
  ## groundwater parameter
  ground_A1 <- 0.02
  ground_B1 <- 1.25
  ground_A2 <- 0.02
  ground_B2 <- 1.25
  ground_A3 <- 0
  ground_Dsw <- 0
  ground_Egwt <- 0
  ground_Wgr <- 0
  ground_Umc <- 0
  
  if (type == "list" & inf_m == "GREEN_AMPT") {
    par <- list(Mann_n_imper, Mann_n_per, S_Imper, S_Per, Route2P, PctRoute2P,
                inf_m, inf_G_S, inf_G_K, inf_G_I, 
                aq_Name, aq_Por, aq_WP, aq_FC, aq_Ksat, aq_Kslope, aq_Tslope, aq_ETu, aq_ETs, aq_Seep, aq_Ebot, aq_Egw, aq_Umc, aq_ETupat,
                ground_A1, ground_B1, ground_A2, ground_B2, ground_A3, ground_Dsw, groud_Egwt)  
  } else if (type == "data.frame" & inf_m == "GREEN_AMPT") {
    par <- data.frame(subcat_area  = c(1, "subcatchment", "gis"),
                      subcat_imper = c(1, "subcatchment", "gis"), 
                      subcat_width = c(1, "subcatchment", "gis"),
                      subcat_slope = c(1, "subcatchment", "gis"),
                      subcat_curb  = c(1, "subcatchment", "gis"),
                      
                                       
      Mann_n_imper = c(Mann_n_imper, "surface", "expert"), 
	                    Mann_n_per = c(Mann_n_per, "surface", "expert"), 
                      S_Imper = c(S_Imper, "surface", "expert"), S_Per = c(S_Per, "surface", "expert"), 
                      Route2P = c(Route2P, "surface", "expert"),
                      PctRoute2P = c(PctRoute2P, "surface", "expert"), 
                      inf_m = c(inf_m, "infiltration", "expert"), inf_G_S=c(inf_G_S, "infiltration", "expert"), 
                      inf_G_K=c(inf_G_K, "infiltration", "expert"), inf_G_I=c(inf_G_I, "infiltration", "expert"), 
                      aq_Name=c(aq_Name, "aquifer", "expert"), aq_Por=c(aq_Por, "aquifer", "expert"), aq_WP=c(aq_WP, "aquifer", "expert"), 
                      aq_FC=c(aq_FC, "aquifer", "expert"), aq_Ksat=c(aq_Ksat, "aquifer", "expert"), aq_Kslope=c(aq_Kslope, "aquifer", "expert"), 
                      aq_Tslope=c(aq_Tslope, "aquifer", "expert"), aq_ETu=c(aq_ETu, "aquifer", "expert"), aq_ETs=c(aq_ETs, "aquifer", "expert"), 
                      aq_Seep=c(aq_Seep, "aquifer", "expert"), aq_Ebot=c(aq_Ebot, "aquifer", "expert"), aq_Egw=c(aq_Egw, "aquifer", "expert"), 
                      aq_Umc=c(aq_Umc, "aquifer", "expert"), aq_ETupat=c(aq_ETupat, "aquifer", "expert"),
                      ground_A1=c(ground_A1, "groundwater", "expert"), ground_B1=c(ground_B1, "groundwater", "expert"), 
                      ground_A2=c(ground_A2, "groundwater", "expert"), ground_B2=c(ground_B2, "groundwater", "expert"), 
                      ground_A3=c(ground_A3, "groundwater", "expert"), ground_Dsw=c(ground_Dsw, "groundwater", "expert"), 
                      ground_Egwt=c(ground_Egwt, "groundwater", "expert"), ground_Wgr = c(ground_Wgr, "groundwater", "expert"),
                      ground_Umc = c(ground_Umc, "groundwater", "expert")) 
    rownames(par) <- c("value", "group", "source")
  } else if (type == "data.frame" & inf_m == "HORTON") {
    par <- data.frame(Mann_n_imper = c(Mann_n_imper, "surface", "expert"), 
                      Mann_n_per = c(Mann_n_per, "surface", "expert"), 
                      S_Imper = c(S_Imper, "surface", "expert"), S_Per = c(S_Per, "surface", "expert"), 
                      Route2P = c(Route2P, "surface", "expert"),
                      PctRoute2P = c(PctRoute2P, "surface", "expert"), 
                      inf_MaxRate = c(inf_MaxRate, "infiltration", "expert"), inf_MinRate=c(inf_MinRate, "infiltration", "expert"), 
                      inf_Decay=c(inf_Decay, "infiltration", "expert"), inf_DryTime=c(inf_DryTime, "infiltration", "expert"), 
                      inf_MaxInfil=c(inf_MaxInfil, "infiltration", "expert"),
                      aq_Name=c(aq_Name, "aquifer", "expert"), aq_Por=c(aq_Por, "aquifer", "expert"), 
                      aq_WP=c(aq_WP, "aquifer", "expert"), 
                      aq_FC=c(aq_FC, "aquifer", "expert"), aq_Ksat=c(aq_Ksat, "aquifer", "expert"), 
                      aq_Kslope=c(aq_Kslope, "aquifer", "expert"), 
                      aq_Tslope=c(aq_Tslope, "aquifer", "expert"), aq_ETu=c(aq_ETu, "aquifer", "expert"),
                      aq_ETs=c(aq_ETs, "aquifer", "expert"), 
                      aq_Seep=c(aq_Seep, "aquifer", "expert"), aq_Ebot=c(aq_Ebot, "aquifer", "expert"), 
                      aq_Egw=c(aq_Egw, "aquifer", "expert"), 
                      aq_Umc=c(aq_Umc, "aquifer", "expert"), aq_ETupat=c(aq_ETupat, "aquifer", "expert"),
                      ground_A1=c(ground_A1, "groundwater", "expert"), ground_B1=c(ground_B1, "groundwater", "expert"), 
                      ground_A2=c(ground_A2, "groundwater", "expert"), ground_B2=c(ground_B2, "groundwater", "expert"), 
                      ground_A3=c(ground_A3, "groundwater", "expert"), ground_Dsw=c(ground_Dsw, "groundwater", "expert"), 
                      ground_Egwt=c(ground_Egwt, "groundwater", "expert"), ground_Wgr = c(ground_Wgr, "groundwater", "expert"),
                      ground_Umc = c(ground_Umc, "groundwater", "expert")) 
    
  } else {
    stop("need type and infiltration method")
  }
  return(par) 
}