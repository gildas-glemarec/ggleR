#' Fix the catch quantification export format
#' @param x Path to the catch quantification files sorted by year or by vessel
#' @param spp_list A species list must be provided. The format is an R list of character vectors. The names must correspond to the ones used in BlackBox Analyzer Catch Quantification records.
#' #' @param incl.fish Include info on fish catches (defaults to FALSE)
#' @return a dataset
#' @export
fix.CQ <- function(x = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/blackbox extractions/catch_quantification/",
                   spp_list = list(),
                   incl.fish = FALSE){
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

      spp_list <<- tibble::lst(
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
  y <- data.table::rbindlist(list_CQdata, fill = TRUE)

  ## Format input data and merge #----
  data.table::setDT(y, key = 'SpecieslistId')
  data.table::setnames(y,
                       old = c('HarbourNumber',
                               'HaulNo',
                               'Species',
                               'Tags',
                               'MeasurementTimeLocal',
                               'Longitude',
                               'Latitude',
                               'CatchCategory',
                               'Remark',
                               'CreatedBy'),
                       new = c('vessel',
                               'haul',
                               'spp',
                               'status',
                               'time.bc',
                               'lon.bc',
                               'lat.bc',
                               'seen_drop',
                               'comments',
                               'name')
  )
  ## Update species groups and classes
  y[, SpeciesGroup := dplyr::case_when(
    spp %in% is.bird ~ 'Bycatch',
    spp %in% is.mammal ~ 'Bycatch',
    spp %in% is.elasmo ~ 'Bycatch',
    spp %in% is.fish ~ 'Catch',
    spp %in% is.not.id ~ 'Other',
    .default = NA_character_)][, SpeciesClass := dplyr::case_when(
      spp %in% is.bird ~ 'Bird',
      spp %in% is.mammal ~ 'Mammal',
      spp %in% is.elasmo ~ 'Elasmobranch',
      spp %in% is.fish ~ 'Fish',
      spp %in% is.not.id ~ 'Other',
      .default = NA_character_)]

  ## Remove rows with no info on Vessel ID & remove Havfisken #----
  y <- y[!y$vessel == "",]
  y <- y[!is.na(y$vessel),]
  y <- y[!y$vessel == "HAV01",]

  y[,c("TripId","FishingTripId","FishingActivityId","ReferenceCode",
       "MeshSize","MeasurementTimeUTC","Length","Weight",
       "Volume","VolumeUnit","State","GpsTimeUtc","CreatedTime",
       "CameraId") := NULL]

  y$date <- y$time.bc
  y$time.bc <- lubridate::dmy_hms(y$time.bc)
  y$Date <- as.Date(lubridate::dmy_hms(y$date))
  data.table::setorderv(y, cols = c("vessel","time.bc"), c(1, 1))

  ## Create IDhaul  #----
  ### Be aware that if the haul crosses 00:00, then the date of
  ### the bycatch event and the IDhaul haul might be different
  y$preID <- paste(y$vessel, substr(y$FishingActivity, 1, 10), sep = ".")
  y$IDhaul <- paste(y$preID, y$haul, sep = ".")
  y$IDhaul <- as.factor(y$IDhaul)

  ## Rarely, there only one entry in CQ (i.e. one image) for multiple animals
  ## of the same spp/status. In this case we need to duplicate that row by
  ## the number in the var "Count"
  y <- rbind(y,y[which(y$Count>1),])

  ## Create IDbc (and IDcatch.sub if incl.fish = TRUE) #----
  rnum <- as.numeric(rownames(y))
  keep <- c("IDhaul", "SpeciesGroup","IDevent")

  ### Create IDevent #----
  y <- y[, ID3 := data.table::frank(.I, ties.method = "first")
         , by = IDhaul][, IDevent := paste(IDhaul, "event", ID3, sep = ".")]
  y <- y[, ID3:=NULL]
  y$IDevent <- as.factor(y$IDevent)

  ### Create IDbc #----
  tmp.bc <- y[, ..keep][SpeciesGroup %in% c("Bycatch")][
    , ID3 := data.table::frank(.I, ties.method = "dense"), by = IDhaul][
      , IDbc := paste(IDhaul, ID3, sep = ".")]
  y <- merge(y, subset(tmp.bc, select = c(-ID3,-IDhaul,-SpeciesGroup)),
             by = 'IDevent',
             all.x = TRUE)
  data.table::setorderv(y, cols = c('vessel', 'Date'))
  y$IDbc <- as.factor(y$IDbc)

  ### Create IDcatch.sub #----
  if(incl.fish == TRUE){
  tmp.catch <- data.table::copy(y)[, ..keep][SpeciesGroup %in% c("Catch")][
    , ID3 := data.table::frank(.I, ties.method = "dense"), by = IDhaul][
      , IDcatch.sub := paste(IDhaul, ID3, sep = ".")]
  y <- merge(y, subset(tmp.catch, select = c(-ID3,-IDhaul,-SpeciesGroup)),
             by = 'IDevent',
             all.x = TRUE)
  data.table::setorderv(y, cols = c('vessel', 'Date'))
  y$IDcatch.sub <- as.factor(y$IDcatch.sub)
  }
  y[, IDevent:=NULL]
  y[, VideoFileName:=NULL]
  return(y)}
