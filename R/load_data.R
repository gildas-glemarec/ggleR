#' Merge a list of similar csv files in a directory
#' Read, format, and merge Notes and Annotations from Black Box Analyzer
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @return A list of data frames
#' @export
load_data <- function(x) {
  files <- dir(x, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(file) data.table::fread(file, encoding = "Latin-1"))
  data.table::rbindlist(tables, fill=TRUE)
}
