report_sections <- c("Element Count",
                     "Pollutant Summary",
                     "Landuse Summary",
                     "Raingage Summary",
                     "Subcatchment Summary",
                     "Node Summary",
                     "Link Summary",
                     "Cross Section Summary",
                     "Analysis Options",
                     #"Rainfall File Summary",
                     #"Rainfall Dependent I/I",
                     #"Control Actions Taken",
                     "Runoff Quantity Continuity", 
                     "Runoff Quality Continuity",
                     "Groundwater Continuity",
                     "Flow Routing Continuity",
                     "Quality Routing Continuity",
                     "Highest Continuity Errors",
                     "Time-Step Critical Elements",
                     "Highest Flow Instability Indexes", 
                     "Routing Time Step Summary", 
                     #"Subcatchment Results",
                     "Subcatchment Runoff Summary",
                     "LID Performance Summary",
                     "Subcatchment Washoff Summary",
                     #"Node Results",
                     "Node Depth Summary", 
                     "Node Inflow Summary", 
                     "Node Flooding Summary", 
                     "Outfall Loading Summary", 
                     #"Link Results",
                     "Link Flow Summary", 
                     "Conduit Surcharge Summary",
                     "Link Pollutant Load Summary",
                     "Pumping Summary",
                     "Groundwater Summary", # example? 
                     "LID Control Summary",
                     "Node Surcharge Summary",
                     "Storage Volume Summary",
                     "Flow Classification Summary")

#' Read SWMM's .rpt file
#'
#' Reads a SWMM .rpt file and creates a list with corresponding results sections.
#' 
#' @param x Name (incl. path) to an report file.
#' @return An object of class `rpt`
#' @param ... optional arguments passed to \code{\link[readr]{read_lines}}
#' @examples  
#' \dontrun{
#' list_of_rpt_results <- read_rpt("model.rpt")
#' } 
#' @rdname read_rpt
#' @export
read_rpt <- function(x, ...) {
  
  # read lines and trimws
  rpt_lines <- readr::read_lines(x) %>% 
    trimws(.) %>% 
    .[!grepl("---------", .)]
  
  # which sections are available?
  section_available <- purrr::map_lgl(report_sections, ~ any(grepl(., x = rpt_lines)))
  
  # last three lines contain analysis_info data
  # remove lines and add analysis_info to final list
  idx_last_lines <- tail(seq_along(rpt_lines), 3)
  analysis_info <- tibble::tibble(value = rpt_lines[idx_last_lines])
  rpt_lines <- rpt_lines[-idx_last_lines]
  
  # if no sections can be found, we got errors
  if (!any(section_available)) {
    message("There are errors.")
    res <- section_to_tbl(x = rpt_lines, section_name = "rpt_error")
    res <- list(error = res, analysis_info = analysis_info)
    # assign class attribute
    class(res) <- "rpt_error"
    return(res)
  }
  
  # subset to available sections only
  report_sections <- report_sections[section_available] %>% 
    # sort vector
    purrr::map_int(., ~ grep(., x = rpt_lines)) %>% 
    purrr::set_names(report_sections[section_available]) %>% 
    sort %>% 
    names
  
  # find section start
  section_start <- purrr::map(report_sections, ~ grep(., x = rpt_lines) - 1) %>%
    purrr::map_if(., ~ identical(., integer(0)) | identical(., numeric(0)), ~ NA) %>% 
    as.integer(.)
  
  # get end per section
  section_end <- purrr::map(section_start[-1] - 3, function(x) {
    # we need this only for sections before analysis options
    if (startsWith(rpt_lines[x], "not just")) x <- x - 7
    return(x)
  }) %>%
    # remove last three lines (analysis)
    c(., length(rpt_lines) - 3) %>% 
    as.integer(.)
  
  # occasionally, swmm produces less than 2 empty lines between sections
  for (i in seq_along(section_end)) {
    x <- section_end[i]
    # check if subsequent line is empty
    while (!identical(rpt_lines[x + 1], "") & x < length(rpt_lines)) {
      x <- x + 1  
    }
    section_end[i] <- x
  }
  
  # remove empty sections (and skip section name)
  section_not_emtpy <- (section_end - section_start > 0)
  section <- list(start = section_start[section_not_emtpy],
                  end = section_end[section_not_emtpy], 
                  name = report_sections[section_not_emtpy])
  
  # create list with sections  
  list_of_sections <- section %>% 
    purrr::transpose() %>% 
    purrr::map( ~ rpt_lines[.$start:.$end]) %>% 
    purrr::set_names(gsub("\\s+|-", "_", base::tolower(section$name)))
  
  # parse sections individually
  res <- purrr::imap(list_of_sections, ~ section_to_tbl(.x, .y)) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections were parsed but empty)
    purrr::discard( ~ nrow(.) < 1)
  
  # add analysis info
  res$analysis_info <- analysis_info
  
  # assign class attribute
  class(res) <- "rpt"
  
  return(res)
  
}