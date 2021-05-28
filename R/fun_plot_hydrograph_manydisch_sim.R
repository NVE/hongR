fun_plot_hydrograph_manydisch_sim <- function(DF, PR, sim, figure_name) {

        png(figure_name, width = 400, height = 200, units = "mm", res = 300)
        common_time <- as.POSIXct(rownames(DF), format = "%Y%m%dT%H%M%S", origin = "1970-01-01", tz = "GMT")
		if (common_time != time(PR)) stop("feil in DF and PR time")
        maxSF   <- max(DF, na.rm = T)
		minSF   <- min(DF, na.rm = T)
        maxPR   <- max(PR, na.rm = T)
        par(mar = c(4, 4, 3, 4) + 0.1)
		
        plot(common_time, DF[,1],
				type = 'l', col = "red",
                ylim = c(minSF, 1.3 * maxSF),
                xaxs = "i", yaxs = "i",
                xlab = "Time", ylab = "Streamflow (cms)")
                #main = paste0(gName, " @ ", sName), cex.main = 0.9)
        #lines(common_date, obsDF, col = "black")
		for (iDF in seq(2, dim(DF)[2])) {
			lines(common_time, DF[,iDF])
		}
		lines(time(sim), sim, col = "blue")
		
        par(new = TRUE)
#        plot(x = common_date, y = PR, 
        
        plot(x = common_time, y = PR, 
                type = "n", ylim = c(5 * maxPR, 0),
                xaxs = "i", yaxs = "i",
                axes = FALSE, xlab = "", ylab = "")
#        segments(x0 = common_date, y0 = rep(0, length(modDF)),
				
        segments(x0 = common_time, y0 = rep(0, length(PR)),
                x1 = common_time, y1 = PR,
                lend = 2, lwd =1, col = "blue")

        yrAxis  <- seq(0, ceiling(maxPR), length.out = 5)
        axis(4, at = yrAxis, labels = paste0(yrAxis))
        
#       mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
        mtext("Precip.", side = 4, line = 2, adj = 1)
        dev.off()
}