#' Fix the catch quantification export format
#' @param x Path to the catch quantification files sorted by year or by vessel
#' @param spp_list A species list must be provided. The format is an R list of character vectors. The names must correspond to the ones used in BlackBox Analyzer Catch Quantification records.
#' @return a dataset
#' @export
fix.CQ <- function(x = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/blackbox extractions/catch_quantification/",
                   spp_list = list()){
  # Species <- NULL
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

    ## Species list #----
    if( length(spp_list) == 0 ){

      message("You should load a species list explicitely.
      Here, the following species list is loaded per default. Check that it is what you need orupdate otherwise.
      spp_list <- list(
      is.bird = c('Ag','Alcidae','Anatidae','At','Bird','Cg','Fg','Ga','Gad',
      'Gar','Gaviidae','Gi','Lar','Larus','Lm','Mb','Mel','Melanitta','Mf','Mn',
      'Pc','Pcr','Pg','Sm','Ua'),
      is.mammal = c('Ba','Hg','La','Mammal','Pp','Pv','Se','Seal'),
      is.elasmo = c('Ar','Do','Gg','Ln','Ma','Mas','Mu','Mustelus','Ray','Rb',
      'Rc','Rm','Sa','Sc','Shark'),
      is.fish = c('Cl','Scsc'),
      is.not.id = c('NA','NI'))"
      )

      spp_list <- tibble::lst(
        is.bird = c('Ag','Alcidae','Anatidae','At','Bird','Cg','Fg','Ga','Gad',
                    'Gar','Gaviidae','Gi','Lar','Larus','Lm','Mb','Mel',
                    'Melanitta','Mf','Mn','Pc','Pcr','Pg','Sm','Ua'),
        is.mammal = c('Ba','Hg','La','Mammal','Pp','Pv','Se','Seal'),
        is.elasmo = c('Ar','Do','Gg','Ln','Ma','Mas','Mu','Mustelus','Ray','Rb',
                      'Rc','Rm','Sa','Sc','Shark'),
        is.fish = c('Cl','Scsc'),
        is.not.id = c('NA','NI'))
      list2env(spp_list, envir = .GlobalEnv)

    }else{

      list2env(spp_list, envir = .GlobalEnv)

    }

    dat <- subset(dat, Species %in% data.frame(group = rep(names(spp_list),
                                                           lengths(spp_list)),
                                               value = unlist(spp_list))$value)
  },
  filenames)
  CQdata <- data.table::rbindlist(list_CQdata, fill = TRUE)
  return(CQdata)}
