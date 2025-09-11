#' Which species are marked at least one in the Catch Quantification tables?
#' @param x Path to the catch quantification files sorted by year or by vessel
#' @return a vector of unique species code names in the CQ tables
#' @export
spp.in.CQ <- function(x = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/blackbox extractions/catch_quantification/"){
  Species <- NULL
  filenames <- list.files(x,
                          full.names = TRUE)
  list_CQdata <- Map(function(x){
    tmp <- readLines(x)
    header <-  grep('^SpecieslistId', tmp, value = TRUE) # extracts the headers (which are NOT the first line!)
    row.header <- as.integer(which(startsWith(tmp, header)))
    last.row.to.remove <- row.header-1
    tmp <- tmp[-c(1:last.row.to.remove)]
    dat <- utils::read.csv2(textConnection(tmp), header=FALSE) # read tmp in as a csv
    names(dat) <- strsplit(header, ';')[[1]] # add headers
    dat <- dat[-1,] ## Remove top row (duplicate of header)
  },
  filenames)
  y <- data.table::rbindlist(list_CQdata, fill = TRUE)
  res <- unique(y$Species)
  return(res)}
