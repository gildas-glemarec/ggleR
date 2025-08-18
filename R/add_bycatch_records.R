#' Add bycatch records from catch quantification data in Analyzer
#' @param x A data frame. Usually the output of BBimport("path_to_files")
#' @param y A data frame. Usually the output of fix.CQ("path_to_bycatch_data")
#' @param alt_spp_list Experimental. Are you providing an alternative species list? Defaults to FALSE.
#' @param rm_errors Defaults to TRUE. Removes the problematic bycatch events (those to fix manually) and prints them
#' @return data.frame object
#' @export
add_bycatch_records <- function(x = data_work,
                                y = NULL,
                                alt_spp_list = F,
                                rm_errors = TRUE){
  d2shore <- i.soak <- soak <- IDhaul <- time.bc <- spp <- colour.name <- path_to_spp_lists <- data_work <- Date <- d <- m <- quarter <- preID <- vessel <- haul <- IDhaul <- ID3 <- time.start <- mesh.colour <- idx <- review.info <- ind <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL
  if(missing(y) | missing(x)) {
    print("You forgot to indicate the path to your EM data file(s).")
  } else {
    if(alt_spp_list == F){
      list2env(spp_list, envir = .GlobalEnv)
    }else{
      if(missing(path_to_spp_lists)) {
        print("You forgot to load the path to your species list file(s)./nFor
              instance, use path_to_spp_lists = 'Q:/scientific-projects/cctv-monitoring/data/species lists/'
              before loading this function")}else{
                list_spp <- ggleR::spp.list(path_to_spp_lists)
                for(i in length(list_spp)){
                  assign(paste0("is.", names(list_spp[i])), unlist(list_spp[i]))
                }
              }
    }
    ## Format input data and merge #----
    data.table::setDT(y, key = 'SpecieslistId')
    data.table::setnames(x = y,
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
    ## Remove rows with no info on Vessel ID & remove Havfisken #----
    y <- y[!y$vessel == "",]
    y <- y[!is.na(y$vessel),]
    y <- y[!y$vessel == "HAV01",]

    y[,c("TripId","FishingTripId","FishingActivityId","ReferenceCode",
         "MeshSize","MeasurementTimeUTC","Length","Weight",
         "Volume","VolumeUnit","State","GpsTimeUtc","CreatedTime",
         "CameraId",NA) := NULL]

    y$date <- y$time.bc
    y$time.bc <- lubridate::dmy_hms(y$time.bc)
    y$Date <- as.Date(lubridate::dmy_hms(y$date))
    data.table::setorderv(y, cols = c("vessel","time.bc"), c(1, 1))
    ## Create IDhaul  #----
    ### Be aware that if the haul crosses 00:00, then the date of
    ### the bycatch event and the IDhaul haul might be different
    y$preID <- paste(y$vessel, substr(y$FishingActivity, 1, 10), sep = ".")
    y$IDhaul <- paste(y$preID, y$haul, sep = ".")
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

    y <- y %>%
      dplyr::group_by(IDhaul) %>%
      dplyr::mutate(ID3 = rank(IDhaul,
                               ties.method = "first"))

    y <- y %>%
      dplyr::mutate(IDbc = paste(IDhaul, ID3, sep = ".")) %>%
      dplyr::select(-ID3)
    y$IDhaul <- as.factor(y$IDhaul)
    y$IDbc <- as.factor(y$IDbc)

    ## Merge Annotations/Notes and Bycatch registrations #----
    data.table::setDT(y, key = 'IDbc')
    data.table::setDT(x, key = 'IDbc')

    merged_data <- merge(y[, c("FishingActivity","haul",
                               # "time.bc.picture","lon.bc","lat.bc",
                               "name","comments","preID",
                               "Date","date","IDhaul","vessel") := NULL],
                         x, # x[, time.bc := NULL],
                         all = TRUE)
    data.table::setcolorder(merged_data, c("review.info", "date", "IDFD",
                                           "IDhaul", "IDbc", "spp", "status",
                                           "netlength", "soak", "std_effort",
                                           "mesh.colour", "vessel"))
    merged_data[, c("y","m","d") := NULL]
    data.table::setorder(merged_data, vessel, time.start)
    merged_data <- merged_data %>%
      dplyr::group_by(IDhaul) %>%
      tidyr::fill(mesh.colour) %>%
      # tidyr::fill(camera) %>%
      # tidyr::fill(quality) %>%
      data.table::as.data.table()

    ## Remove some duplicated rows (when there is at least one event in a haul,
    ## the "activity" row becomes redundant)
    merged_data$ind <- (stringr::str_detect(merged_data$Id, "^a"))
    merged_data <- merged_data[, idx := .N, by = IDhaul][idx == 1 | is.na(review.info) | idx > 1 & ind != TRUE,]
    merged_data[, c("idx","Count","Id","ind") := NULL]

    if(rm_errors == FALSE){
      return(merged_data)
    }else{
      errors1 <- merged_data %>%
        dplyr::filter(colour.name == 'Aqua' & !spp %in% is.elasmo |
                        colour.name == 'Black' & !spp %in% is.mammal |
                        colour.name == 'Blue' & !spp %in% is.bird
                      # | colour.name %in% c('Gray','Gray','Thistle') & !spp %in% is.fish
        )
      errors2 <- merged_data[is.na(merged_data$review.info), ]
      if(dim(errors1)[1]==0 & dim(errors2)[1]==0){
        message("No error detected in the input data. Congratulations!")
      }else{
        ## Print error message #----
        errors <- rbind(errors2, errors1)
        assign("errors", errors, envir = .GlobalEnv)
        utils::View(errors)
        message("
      ####!!!####!!!####!!!####!!!####
      ####!!!####!!!####!!!####!!!####
A dataset with the missing bycatch spp was saved to the workspace (and it is called errors).\nThe missing matches between activity and bycatch data are listed and must be fixed in BB ANalyzer / Catch quantification.\nThen, re-extract the catch quantification, and finally re-run this script.\nON Windows, you can try: write.csv(errors,'Q:/10-forskningsprojekter/faste-cctv-monitoring/data/errors.csv')
      ####!!!####!!!####!!!####!!!####
      ####!!!####!!!####!!!####!!!####")
      }
      if('is.fish' %in% names(spp_list)){
        merged_data <- merged_data[!is.na(merged_data$review.info), ][
          !(colour.name == 'Aqua' & !spp %in% is.elasmo)][
            !(colour.name == 'Black' & !spp %in% is.mammal)][
              !(colour.name == 'Blue' & !spp %in% is.bird)][
                (!colour.name %in% c('Gray','Grey','Thistle') & !spp %in% is.fish)]
      }else{
        merged_data <- merged_data[!is.na(merged_data$review.info), ][
          !(colour.name == 'Aqua' & !spp %in% is.elasmo)][
            !(colour.name == 'Black' & !spp %in% is.mammal)][
              !(colour.name == 'Blue' & !spp %in% is.bird)]
      }
      data.table::setorder(merged_data, vessel, time.start)
      return(merged_data)
    }
  }
}
