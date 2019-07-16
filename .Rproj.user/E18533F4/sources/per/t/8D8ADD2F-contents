#' find_upstream_pipes
#' @param outlet
#' @param pipe TONODE and FROMNODE
#'
find_upstream_pipes <- function(outlet = outlet, pipe = pipe) {
	require(sf)
	#for (iO in outlet) {

	nUpStreamOld <- 0
	UpStream <- which(pipe$TONODE == outlet)
	TMID <- c(outlet, pipe$FROMNODE[UpStream], pipe$TONODE[UpStream])
	TMID <- unique(TMID)
	nUpStreamNew <- length(UpStream)

	while (nUpStreamOld < nUpStreamNew) {
		nUpStreamOld <- length(UpStream)
		UpStream <- which(pipe$TONODE %in% TMID)
		nUpStreamNew <- length(UpStream)
		#print(nUpStreamOld)
		#print(nUpStreamNew)
		#print(TMID)
		TMID <- c(TMID, pipe$FROMNODE[UpStream], pipe$TONODE[UpStream])
		TMID <- unique(TMID)
		#print(Sys.time())
		#print(UpStream)
	}
	return(pipe[UpStream,])

	#}
}
