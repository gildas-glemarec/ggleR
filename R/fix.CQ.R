#' Fix the catch quantification export format
#' @param x Path to the catch quantification files sorted by year or by vessel
#' @return a dataset
#' @export
fix.CQ <- function(x = "Q:/scientific-projects/cctv-monitoring/data/blackbox extractions/catch_quantification/"){
  Species <- NULL
  `%notin%` <- Negate(`%in%`)
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
    dat <- dat[-1,]
    dat <- subset(dat, Species %notin% c("Cl"))
  },
  filenames)
  CQdata <- data.table::rbindlist(list_CQdata)
  return(CQdata)}
