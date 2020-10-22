# read pest tpl file and return as a data.frame for hbv paramters
read_pest_tpl <- function(tpl_fil) {
#tpl_fil <- "soil_parameters.tpl"
tpl_str <- readLines(tpl_fil, n = -1)
par_str <- unlist(strsplit(tpl_str[-1], " "))
loc_sep <- which(par_str == "#")
if ((length(loc_sep)%% 2) != 0) stop("error in tpl file")
par_names <- NULL
for (ipar in seq(1, length(loc_sep), 2)) {
	par_names <- c(par_names, par_str[loc_sep[ipar]+1]) 	

}
return(par_names)
}
