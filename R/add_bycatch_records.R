#' Add bycatch records from catch quantification data in Analyzer
#' @param x A data frame. Usually the output of BBimport("path_to_files")
#' @param y A data frame. Usually the output of fix.CQ("path_to_bycatch_data")
#' @param spp_list A species list must be provided. The format is an R list of character vectors. The names must correspond to the ones used in BlackBox Analyzer Catch Quantification records.
#' @param rm_errors Defaults to TRUE. Removes the problematic bycatch events (those to fix manually) and prints them
#' @param incl.fish Incl. the catches not recorded both as notes and in catch quantification. this is the case for some fish species. Defaults to FALSE
#' @return data.frame object
#' @export
add_bycatch_records <- function(x = data_work,
                                y = NULL,
                                spp_list = list(),
                                rm_errors = TRUE,
                                incl.fish = FALSE){
  is.elasmo <- is.bird <- is.mammal <- note.type <- SpeciesGroup <- SpeciesClass <- IDevent <- IDbc <- IDcatch.sub <- d2shore <- i.soak <- soak <- IDhaul <- time.bc <- spp <- colour.name <- path_to_spp_lists <- data_work <- Date <- d <- m <- quarter <- preID <- vessel <- haul <- IDhaul <- ID3 <- time.start <- mesh.colour <- idx <- review.info <- ind <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL
  if(missing(y) | missing(x)) {
    print("You forgot to indicate the path to your EM data file(s).")
  }
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
    .default = 'Other')][, SpeciesClass := dplyr::case_when(
      spp %in% is.bird ~ 'Bird',
      spp %in% is.mammal ~ 'Mammal',
      spp %in% is.elasmo ~ 'Elasmobranch',
      spp %in% is.fish ~ 'Fish',
      .default = 'Other')]

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
  # y <- y %>%
  #   tidyr::separate(Date, c("y","m","d")) %>%
  #   tidyr::unite(col = Date, c(d,m,y), sep = "-") %>%
  #   ## Create variable "preID" (as vessel.dd-mm-yyyy)
  #   tidyr::unite(preID, c(vessel, Date), sep = ".", remove = FALSE) %>%
  #   dplyr::mutate(IDhaul = paste(preID, haul, sep = "."))

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
    tmp.catch <- y[, ..keep][SpeciesGroup %in% c("Catch")][
      , ID3 := data.table::frank(.I, ties.method = "dense"), by = IDhaul][
        , IDcatch.sub := paste(IDhaul, ID3, sep = ".")]
    y <- merge(y, subset(tmp.catch, select = c(-ID3,-IDhaul,-SpeciesGroup)),
               by = 'IDevent',
               all.x = TRUE)
    data.table::setorderv(y, cols = c('vessel', 'Date'))
    y$IDcatch.sub <- as.factor(y$IDcatch.sub)
  }
  y[, IDevent:=NULL]

  ## Merge Annotations/Notes and Bycatch/Catch registrations #----
  ### Without fish catches #----
  if(incl.fish == FALSE){
    data.table::setDT(y, key = 'IDbc')
    data.table::setDT(x, key = 'IDbc')
    merged_data <- merge(y[, c("FishingActivity","haul",
                               "name","comments","preID",
                               "Date","date","IDhaul","vessel") := NULL],
                         x,
                         all = TRUE)
  }else{ ### With fish catches #----

    y.Bycatch <- y[SpeciesGroup == 'Bycatch'][, c("IDcatch.sub","FishingActivity",
                                                  "haul", "name","comments","preID",
                                                  "Date","date","IDhaul",
                                                  "vessel") := NULL]
    data.table::setDT(y.Bycatch, key = 'IDbc')
    data.table::setDT(x, key = 'IDbc')
    merged_data <- merge(y.Bycatch,
                         x,
                         all = TRUE)
    y.Catch <- y[SpeciesGroup == 'Catch'][, c("FishingActivity",
                                              "haul", "name","comments","preID",
                                              "Date","date","IDhaul","vessel",
                                              "IDbc") := NULL]
    data.table::setDT(y.Catch, key = 'IDcatch.sub')
    data.table::setDT(x, key = 'IDcatch.sub')
    cols_to_update <- c("SpecieslistId", "time.bc", "Count", "spp", "seen_drop",
                        "status", "lon.bc", "lat.bc", "VideoFileName",
                        "SpeciesGroup", "SpeciesClass")
    for (col in cols_to_update) {
      merged_data[y.Catch, (col) := get(paste0("i.", col)), on = "IDcatch.sub"]
    }
    merged_data[, 'IDcatch.sub' := NULL]
  }
  data.table::setcolorder(merged_data, c("review.info", "date", "IDFD",
                                         "IDhaul", "IDbc", "spp",
                                         "status", "netlength", "soak",
                                         "std_effort", "mesh.colour", "vessel"))
  merged_data[, c("y","m","d") := NULL]
  data.table::setorder(merged_data, vessel, time.start)
  merged_data <- merged_data %>%
    dplyr::group_by(IDhaul) %>%
    tidyr::fill(mesh.colour) %>%
    data.table::as.data.table()

  ## Remove some duplicated rows (when there is at least one event in a haul,
  ## the "activity" row becomes redundant)
  merged_data$ind <- (stringr::str_detect(merged_data$Id, "^a"))
  merged_data <- merged_data[, idx := .N, by = IDhaul][
    idx == 1 | is.na(review.info) | idx > 1 & ind != TRUE,]
  merged_data[, c("idx","Count","Id","ind") := NULL]

  ## Error list #----
  if(rm_errors == FALSE){
    return(merged_data)
  }else{
    errors1 <- merged_data[
      (colour.name == "Aqua" & !spp %in% is.elasmo) |
        (colour.name == "Black" & !spp %in% is.mammal) |
        (colour.name == "Blue" & !spp %in% is.bird) |
        (colour.name %in% c("Gray", "Grey", "DarkKhaki", "Thistle",
                            "SaddleBrown") &
           !note.type %in% c("Cod", "Lumpsucker", "Flatfish", "Sole",
                             "Mackerel")),
    ]
    errors2 <- merged_data[is.na(merged_data$review.info), ]
    if(dim(errors1)[1]==0 & dim(errors2)[1]==0){
      message("No error detected in the input data. Congratulations!")
    }else{
      ### Print error message #----
      errors <- rbind(errors2, errors1)
      assign("errors", errors, envir = .GlobalEnv)
      utils::View(errors)
      message("
      ####!!!####!!!####!!!####!!!####
      ####!!!####!!!####!!!####!!!####
A dataset with the missing bycatch spp was saved to the workspace (and it is
called errors).\nThe missing matches between activity and bycatch data are
listed and must be fixed in BB Analyzer / Catch quantification.\nThen,
re-extract the catch quantification, and finally re-run this script.\nOn
Windows, you can try: write.csv(errors,
'Q:/10-forskningsprojekter/faste-cctv-monitoring/data/errors.csv')
      ####!!!####!!!####!!!####!!!####
      ####!!!####!!!####!!!####!!!####")
    }
    merged_data <- merged_data[!is.na(merged_data$review.info), ][
      (colour.name == "Aqua" & !spp %in% is.elasmo) |
        (colour.name == "Black" & !spp %in% is.mammal) |
        (colour.name == "Blue" & !spp %in% is.bird) |
        (colour.name %in% c("Gray", "Grey", "DarkKhaki", "Thistle",
                            "SaddleBrown") &
           !note.type %in% c("Cod", "Lumpsucker", "Flatfish", "Sole",
                             "Mackerel")),
          ]
    data.table::setorder(merged_data, vessel, time.start)
    return(merged_data)
  }
}

