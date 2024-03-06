#' Merge a list of similar csv files in a directory
#' Read, format, and merge Notes and Annotations from Black Box Analyzer
#' @param x path to the directory where the logbook/salesnotes are stored as .csv
#' @return A list of data frames
#' @export
load_data <- function(x) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  data.table::rbindlist(tables, fill=TRUE)
}
