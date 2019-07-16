#' find nearestXY
#'
#' @param x x coordinate of point lon or utm_e
#' @param y y coordinate of point lat or utm_n
#' @param X x coordinate of points lon or utm_e
#' @param Y y coordinate of points lat or utm_n
#'
#' @keywords data
#' @export
#' @examples
#' nearestXY()

nearestXY <- function(x, y, X = X, Y = Y, n = 9) {
	xx <- matrix(rep(X, length(Y)), nrow = length(Y))
	yy <- matrix(rep(Y, length(X)), ncol = length(X))
	dis <- (xx-x)^2 + (yy-y)^2
	nearest <- order(dis, decreasing = FALSE)[1:n]
	nx <- xx[nearest]
	ny <- yy[nearest]
	return(cbind(nx, ny))
}
