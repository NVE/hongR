read_hbv_landscape <- function(landscape_file = landscape_file, n_landsurface = 2, n_rr_stat = 4, n_tm_stat = 4) {
  #landscape_file <- "/data05/GlobalHydroPressure/hbv/10000000/hbv/001000m_Day_Month_APHRODITE/landscape.txt"

  landscape_lines <- readLines(landscape_file)
  ncols <- as.integer(gsub("ncols", "", landscape_lines[1]))
  nrows <- as.integer(gsub("nrows", "", landscape_lines[2]))  
  xllcorner <- as.numeric(gsub("xllcorner", "", landscape_lines[3]))
  yllcorner <- as.numeric(gsub("yllcorner", "", landscape_lines[4]))
  cellsize <- as.integer(gsub("cellsize", "", landscape_lines[5]))	  
  NODATA_value <- as.numeric(gsub("NODATA_value", "", landscape_lines[6]))
  numberofelements <- as.integer(gsub("# Number of landscape elements :", "", landscape_lines[7]))
  info <- as.numeric(unlist(strsplit(x = landscape_lines[8:length(landscape_lines)], split = " ")))
  info_matrix <- matrix(info, nrow = length(landscape_lines) - 7, byrow = TRUE)
  info_dfr <- as.data.frame(info_matrix)

  landscape_common_name <- c("landIndex", "geoIndex", "modStruct",
        "elementArea", "elementElevation", "elementSlopeLength", "elementSlopeAngle", "elementAspect", "elementFlowDir",
        "lakePercent", "glacierPercent", "glacerSurfaceElevation", "glacierIceThick")
  landsurface_name <- rep(c("land", "soil", "percentage"), n_landsurface)
  rr_stat_name <- rep(c("rr_stat", "w_rr_stat"), n_rr_stat)
  tm_stat_name <- rep(c("tm_stat", "w_tm_stat"), n_tm_stat)

  colnames(info_dfr) <- c(landscape_common_name, landsurface_name, rr_stat_name, tm_stat_name)
  
  landscape <- list(ncols = ncols, nrows = nrows,
                  xllcorner = xllcorner, yllcorner = yllcorner,
                  cellsize = cellsize, NODATA_value = NODATA_value, numberofelements = numberofelements,
                  landscape_info = info_dfr)
  return(landscape)

}

