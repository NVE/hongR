#' write_pest
#' @param x
#' @param file
#' @export

write_pest <- function(x, file) {
  # check class and required elements
  stopifnot(inherits(x, "pest"))

  # use sink to write sections to
  sink(file = file)

  # for each setion ...
  for (section in names(x)) {

    # write section name
    #cat(paste0("[", toupper(section), "]"), sep = "\n")
    inn_string <- paste(names(x[[section]]), sep = "\t", collapse = "\t")

    # write the data without names
    utils::write.table(x = x[section],
                       quote = FALSE,
                       na = "",
                       row.names = FALSE,
                       col.names = FALSE)

    # write newline to separate sections
    #cat("\n")

  }

  # close sink
  sink()

}
