#' find_wrong_pipes
#' @param data.frame and has two columns, TONODE and FROMNODE
#' @author lihong2291@gmail.com
#' @export
#'
find_wrong_pipes <- function(pipe = pipe) {
  dup_fm <- pipe$FROMNODE[duplicated(pipe$FROMNODE)]
  dup_pipe <- pipe[which(pipe$FROMNODE %in% dup_fm),]

  return(dup_pipe)

}
