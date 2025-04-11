#' Load a species list - Experimental
#' @param path_to_spp_lists Path to the a directory where csv files with the list of bycatch species is kept
#' @return list of vectors of species groups
#' @examples
#' # This function is called in add_bycatch_records and is not supposed to be used on its own.
#' # For testing purpose: Create bycatch species "lists" (as in ICES spp list, not R list!)
#' using default for bycatch monitoring in DK 2024
#' spp.list <- spp.list(path_to_spp_lists =
#'   "Q:/scientific-projects/cctv-monitoring/data/species lists/")
#' @export
spp.list <- function(path_to_spp_lists = path_to_spp_lists){
  all_files <- list.files(path_to_spp_lists,
                          full.names = TRUE,
                          recursive = FALSE)
  filenames <- all_files[!file.info(all_files)$isdir]
  list_spp <- lapply(filenames,
                     utils::read.csv,
                     header=TRUE, sep = ";",
                     stringsAsFactors = FALSE, quote = "")
  names(list_spp) <- tolower(gsub(".*/(.+).csv.*", "\\1", filenames))
  return(list_spp)
}
