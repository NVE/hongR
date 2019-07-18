#' find_up_or_down_pipes
#' @param outlet
#' @param pipe TONODE and FROMNODE
#' @param dir "up" or "down"
#'
find_up_or_down_pipes <- function(outlet = outlet, dir = "up", pipe = pipe) {
  if (dir == "up") {
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

  } else if (dir == "down") {
    nDownStreamOld <- 0
    DownStream <- which(pipe$FROMNODE == outlet)
    nDownStreamNew <- 1
    outlet_new <- pipe$TONODE[DownStream]

    while (nDownStreamOld < nDownStreamNew) {
      new_pipe <- which(pipe$FROMNODE == outlet_new)
      nDownStreamOld <- length(DownStream)

      DownStream <- c(DownStream, new_pipe)
      nDownStreamNew <- length(DownStream)
      outlet_new <- pipe$TONODE[new_pipe]
      #print(nDownStreamOld)
      #print(nDownStreamNew)
      #print(TMID)
    }
    return(pipe[DownStream,])

  } else {
    stop("dir must be up or down")
  }


}
