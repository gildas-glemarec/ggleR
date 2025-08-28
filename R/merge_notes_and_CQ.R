#' Add bycatch records (and catch records if incl.fish = TRUE) from the catch
#' quantification dataset in Analyzer to the Notes and Annotations dataset
#' @param x A data frame. Usually the output of BBimport("path_to_files")
#' @param y A data frame. Usually the output of fix.CQ("path_to_bycatch_data")
#' @param spp_list A species list must be provided. The format is an R list of character vectors. The names must correspond to the ones used in BlackBox Analyzer Catch Quantification records.
#' @param incl.fish Include info on fish catches (defaults to FALSE)
#' @param rm_errors Defaults to TRUE. Removes the problematic bycatch events (those to fix manually) and prints them
#' @return data.frame object
#' @export
merge_notes_and_CQ <- function(x = NotesAnnotations,
                               y = CatchQuantification,
                               spp_list = list(),
                               incl.fish = FALSE,
                               rm_errors = TRUE){

  is.elasmo <- is.bird <- is.mammal <- note.type <- SpeciesGroup <- SpeciesClass <- IDevent <- IDbc <- IDcatch.sub <- d2shore <- i.soak <- soak <- IDhaul <- time.bc <- spp <- colour.name <- path_to_spp_lists <- data_work <- Date <- d <- m <- quarter <- preID <- vessel <- haul <- IDhaul <- ID3 <- time.start <- mesh.colour <- idx <- review.info <- ind <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL
  if(missing(y) | missing(x)) {
    print("You forgot to indicate the path to your EM data file(s).")
  }

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
  }else{
    ### With fish catches #----
    y.Bycatch <- data.table::copy(y)[SpeciesGroup == 'Bycatch'][, c("IDcatch.sub","FishingActivity",
                                                                    "haul", "name","comments","preID",
                                                                    "Date","date","IDhaul",
                                                                    "vessel") := NULL]
    data.table::setDT(y.Bycatch, key = 'IDbc')
    data.table::setDT(x, key = 'IDbc')
    merged_data <- merge(y.Bycatch,
                         x,
                         all = TRUE)
    y.Catch <- data.table::copy(y)[SpeciesGroup == 'Catch'][, c("FishingActivity",
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

    errors1 <- merged_data[(colour.name == "Aqua" & !spp %in% is.elasmo) |
                             (colour.name == "Black" & !spp %in% is.mammal) |
                             (colour.name == "Blue" & !spp %in% is.bird) |
                             (colour.name %in% c("Gray", "Grey",
                                                 "DarkKhaki",
                                                 "Thistle",
                                                 "SaddleBrown") &
                                !note.type %in% c("Lumpsucker",
                                                  "Cod",
                                                  "Mackerel",
                                                  "Flatfish", "Sole")),]

    errors2 <- merged_data[is.na(merged_data$review.info), ]

    if( dim(errors1)[1]==0 & dim(errors2)[1]==0 ){
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
  }
  merged_data <- merged_data[!is.na(merged_data$review.info), ][
    !(colour.name == "Aqua" & !spp %in% is.elasmo) &
      !(colour.name == "Black" & !spp %in% is.mammal) &
      !(colour.name == "Blue" & !spp %in% is.bird) &
      !(colour.name %in% c("Gray", "Grey",
                           "DarkKhaki",
                           "Thistle",
                           "SaddleBrown") &
          !note.type %in% c("Lumpsucker",
                            "Cod",
                            "Mackerel",
                            "Flatfish", "Sole")),]
  data.table::setorder(merged_data, vessel, time.start)
  return(merged_data)
}

