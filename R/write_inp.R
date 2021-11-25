write_inp <- function(x, file) {
   # check class and required elements
    stopifnot(inherits(x, "inp"))
    
    # use sink to write sections to
    sink(file = file)
    
    # for each setion ...
    for (section in names(x)) {
        
        # write section name
        cat(paste0("[", toupper(section), "]"), sep = "\n")
        inn_string <- paste(names(x[[section]]), sep = "\t", collapse = "\t")
        
		## hong add column name
        cat(paste(";;", inn_string, sep = ""), sep = "\n")
		cat(";;--------------", sep = "\n")
        # write the data without names 
        utils::write.table(x = x[section],
                           quote = FALSE,
                           na = "",
                           row.names = FALSE, 
                           col.names = FALSE)
        
        # write newline to separate sections
        cat("\n")
        
    }
    
    # close sink
    sink()
  
}
