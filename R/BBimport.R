#' Extracts Notes and Annotations
#' Read, format, and merge Notes and Annotations from Black Box Analyzer
#' @param x EM data annotations/notes with geographic coordinates in decimal as lon/lat
#' @param by.year Are the files sorted by year (default)?
#' @param incl.fish Should the bycatch data also include (targeted) fish species (e.g. lumpfish, mackerel, etc.)? Defaults to FALSE
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @import data.table
#' @export
BBimport <- function(x = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/blackbox extractions/annotations_notes/",
                     by.year = TRUE,
                     incl.fish = FALSE) {
  Gear.type <- note.type <- review.info <- Id <- d <- m <- y <- Activity.type <- Note.type <- Color.name <- colour.name <- Haul.no <- Mesh.color <- Vesselid <- vessel <- time.start <- haul_number <- IDFD <- IDhaul <- IDevent <- haul.lon.start <- haul.lon.stop <- haul.lat.start <- haul.lat.stop <- Distance..m. <- Soaking.time..h. <- Review.info <- gps <- Start.longitude <- End.longitude <- Start.latitude <- End.latitude <- time.stop <- Note <- Activity.comment <- mitigation <- mitigation_type <- ID3 <- IDevent <- Treatment.Group <- NULL
  `%notin%` <- Negate(`%in%`)
  ## Get all files together as a list #----
  if( by.year == FALSE ){
    all_files <- list.files(x, pattern = "^[A-Za-z]", full.names = TRUE,
                            recursive = FALSE)
    filenames <- all_files[!file.info(all_files)$isdir]
  } else{
    all_files <- list.files(x, pattern = "^20", full.names = TRUE,
                            recursive = FALSE)
    filenames <- all_files[!file.info(all_files)$isdir]
  }
  list_BBdata <- lapply(filenames,
                        utils::read.csv,
                        header=TRUE, sep = ";",
                        stringsAsFactors = FALSE, quote = "")
  names(list_BBdata) <- tolower(gsub(".*/(.+).csv.*", "\\1", filenames))

  ## Map function  #----
  list_BBdata <- Map(function(x){
    if( is.null(x$Treatment.Group) ){ x$Treatment.Group <- NA_integer_ }
    if( is.logical(x$Treatment.Group) ){ x$Treatment.Group <- NA_integer_ }
    x <- x[!x$Vesselid == "",]
    x <- x[!is.na(x$Vesselid),]
    x <- x[!x$Vesselid == "HAV01",]
    ## Keep only notes and annotations #----
    x <- x %>%
      dplyr::filter(substr(Id,1,1) == "a" | substr(Id,1,1) == "n")
    ## Remove the Green/Red and PaleGreen/LightSalmon notes (manual markings of start/stop) #----
    x <- x %>%
      dplyr::filter(Color.name %notin% c('PaleGreen', 'LightSalmon',
                                         'Green', 'Red'))

    x$date <- as.Date(lubridate::dmy_hms(x$Start.time.local..log.))
    x$Start.time.local..log. <- strptime(x$Start.time.local..log.,
                                         "%d-%m-%Y %H:%M:%S")
    x$End.time.local..log. <- strptime(x$End.time.local..log.,
                                       "%d-%m-%Y %H:%M:%S")
    x$time.start <- as.character(x$Start.time.local..log.)
    x$time.stop <- as.character(x$End.time.local..log.)

    x <- x %>%
      dplyr::mutate(gps = if_else(is.na(Start.latitude),
                                  0, 1))
    x <- x[with(x, order(x$Vesselid,x$time.start)),]
    x <- x[!x$Type == "Videofile",]
    x <- x[!x$Type == "Trip",]
    x <- x[!x$Type == "BlackBoxNote",]
    x <- x[!x$Type == "MiniDisc",]
    x$Mesh.color <- as.character(x$Mesh.color)
    x <- x[!x$Color.code == "",]
    x$Start.latitude <- as.numeric(x$Start.latitude) ## in decimal
    x$Start.longitude <- as.numeric(x$Start.longitude) ## in decimal
    x$End.latitude <- as.numeric(x$End.latitude) ## in decimal
    x$End.longitude <- as.numeric(x$End.longitude) ## in decimal

    x <- x %>%
      tidyr::separate(date, c("y","m","d")) %>%
      tidyr::unite(col = date, c(d,m,y), sep = "-") %>%
      dplyr::mutate(Date = date)
    x$IDFD <- paste(x$Vesselid, x$Date, sep = ".")

    x <- x %>%
      dplyr::filter(Activity.type != 'Gear out') %>%
      dplyr::filter(Note.type %notin% c('Anchor 1', 'Anchor 2', 'Start', 'Stop')) %>%
      dplyr::filter(Color.name %notin% c('PaleGreen', 'LightSalmon', 'Green',
                                         'Red', 'MediumTurquoise')) %>%
      dplyr::rename(haul_number = Haul.no) %>%
      dplyr::mutate(Mesh.color = na_if(Mesh.color,"")) %>%
      ## Create IDhaul for Activities (i.e. hauling) #----
      dplyr::mutate(IDhaul = dplyr::case_when(Activity.type == 'Gear in' ~
                                                paste(IDFD, haul_number, sep = "."),
                                              .default = NA)) %>%
      dplyr::mutate(haul.lon.start = NA,
                    haul.lat.start = NA,
                    haul.lon.stop = NA,
                    haul.lat.stop = NA) %>%
      dplyr::mutate(haul.lon.start = dplyr::case_when(Activity.type == 'Gear in'~
                                                        Start.longitude,.default = NA_integer_),
                    haul.lat.start = dplyr::case_when(Activity.type == 'Gear in'~
                                                        Start.latitude,.default = NA_integer_),
                    haul.lon.stop = dplyr::case_when(Activity.type == 'Gear in'~
                                                       End.longitude,.default = NA_integer_),
                    haul.lat.stop = dplyr::case_when(Activity.type == 'Gear in'~
                                                       End.latitude,.default = NA_integer_)) %>%
      ## Copy the haul characteristics (stored as Activity down in the corresponding Notes) #----
      dplyr::arrange(Vesselid, time.start) %>%
      tidyr::fill(IDhaul)
    ## Fix the problem where the first pinger appears before the beginning of the activity #----
    for(i in 1:(length(x$Id)-1)){
      if(x$Type[i] == 'Note' &
         x$Type[i+1] == 'Activity' &
         x$time.start[i] < x$time.start[i+1] &
         x$time.stop[i] > x$time.start[i+1]){
        # x <- x[-i, ] ##Removes the row
        x$IDhaul[i] <- x$IDhaul[i+1]
        x$haul_number[i] <- x$haul_number[i+1]
        x$Gear.type[i] <- x$Gear.type[i+1]
        x$Distance..m.[i] <- x$Distance..m.[i+1]
        x$Soaking.time..h.[i] <- x$Soaking.time..h.[i+1]
        x$Mesh.color[i] <- x$Mesh.color[i+1]
        x$Review.info[i] <- x$Review.info[i+1]
        x$haul.lon.start[i] <- x$haul.lon.start[i+1]
        x$haul.lat.start[i] <- x$haul.lat.start[i+1]
        x$haul.lon.stop[i] <- x$haul.lon.stop[i+1]
        x$haul.lat.stop[i] <- x$haul.lat.stop[i+1]
      }
    }

    x <- x %>%
      dplyr::group_by(IDhaul) %>%
      tidyr::fill(haul_number) %>%
      tidyr::fill(Gear.type) %>%
      tidyr::fill(Distance..m.) %>%
      tidyr::fill(Soaking.time..h.) %>%
      tidyr::fill(Mesh.color) %>%
      tidyr::fill(Review.info) %>%
      tidyr::fill(haul.lon.start) %>%
      tidyr::fill(haul.lat.start) %>%
      tidyr::fill(haul.lon.stop) %>%
      tidyr::fill(haul.lat.stop) %>%
      dplyr::ungroup() %>%
      dplyr::select(vessel = Vesselid,
                    date,
                    review.info = Review.info,
                    mesh.colour = Mesh.color,
                    netlength = Distance..m.,
                    soak = Soaking.time..h.,
                    haul_number = haul_number,
                    Id,
                    IDhaul,
                    IDFD,
                    gps,
                    haul.lon.start,
                    haul.lat.start,
                    haul.lon.stop,
                    haul.lat.stop,
                    lat.start = Start.latitude,
                    lon.start = Start.longitude,
                    lat.stop = End.latitude,
                    lon.stop = End.longitude,
                    time.start,
                    time.stop,
                    colour.name = Color.name,
                    treatment = Treatment.Group,
                    note.type = Note.type,
                    comments = Note,
                    add.comments = Activity.comment
      ) %>%
      dplyr::mutate(review.info = dplyr::case_when(
        is.na(review.info) & is.na(mesh.colour) ~ 0,
        is.na(review.info) & !is.na(mesh.colour) ~ 1,
        is.na(review.info) & !is.na(colour.name) ~ 1,
        .default = review.info)) %>%
      dplyr::group_by(IDhaul) %>%
      dplyr::mutate(review.info = if_else(rep(any(review.info == 1),
                                              dplyr::n()), 1, review.info)
      ) %>%
      dplyr::ungroup() %>%
      ## Is there  mitigation in place? #----
      dplyr::mutate(mitigation = dplyr::case_when(
        colour.name == "Yellow" ~ '1',
        treatment == 1 ~ '1',
        .default = NA)) %>%
      dplyr::group_by(IDhaul) %>%
      tidyr::fill(mitigation) %>%
      dplyr::mutate(mitigation = dplyr::if_else(is.na(mitigation),
                                                "0",
                                                mitigation)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mitigation_type = dplyr::case_when(
        mitigation == '1' & colour.name == "Yellow" ~ 'pinger',
        mitigation == '1' &
          add.comments %in% add.comments[grepl(paste(c("(^|[^a-zA-Z])tt([^a-zA-Z]|$)",
                                                       "thin thread",
                                                       "thin-thread"),
                                                     collapse = "|"),
                                               add.comments)] ~ 'thin-twine nets',
        mitigation == '1' &
          add.comments %in% add.comments[grepl(paste(c("low net", "low-net"),
                                                     collapse = "|"),
                                               add.comments)] ~ 'low nets',
        #colour.name == "colour-to-define" ~ 'some_mitigation_device',
        #colour.name == "colour-to-define" ~ 'other_mitigation',
        .default = NA)) %>%
      dplyr::group_by(IDhaul) %>%
      tidyr::fill(mitigation_type) %>%
      dplyr::mutate(mitigation_type = dplyr::if_else(is.na(mitigation_type),
                                                     "no mitigation",
                                                     mitigation_type)) %>%
      dplyr::ungroup() %>%
      ## Create an event ID #----
      dplyr::group_by(IDhaul) %>%
      dplyr::mutate(ID3 = rank(haul_number,
                               ties.method = "first")) %>% # ID3 = note "number" (rank) per haul (sorted chronologically)
      dplyr::mutate(IDevent = paste(IDhaul, "event", ID3, sep = ".")) %>%
      dplyr::ungroup() %>%
      dplyr::select(-ID3)

    ## Remove duplicated IDhaul if there is at least one note #----
    x <- x %>%
      dplyr::group_by(IDhaul) %>%
      dplyr::filter(if (dplyr::n() > 1) dplyr::row_number() != 1 else TRUE) %>%
      dplyr::ungroup()
  },
  list_BBdata)

  ## Bind the files in the list as one dt #----
  BBdata <- data.table::rbindlist(list_BBdata)

  ## Add variable IDbc (and IDcatch if incl.fish == TRUE) #----
  ### Include only the bycatch groups we are interested in
  # Aqua           Elasmobranch
  # Black          Mammal
  # Blue           Bird
  # Brown          Pearl net stop
  # DarkKhaki      Cod
  # DeepPink       Seal damage
  # Gray           Lumpsucker
  # LawnGreen      Pearl net start
  # Orange         Andet
  # Purple         Plastic
  # SaddleBrown    Flatfish or Sole
  # Thistle        Mackerel
  # Yellow         Pinger

  tmp.bc <- BBdata %>%
    dplyr::select(c(haul_number, IDhaul, IDevent, colour.name, note.type)) %>%
    dplyr::filter(colour.name %in% c("Black","Blue","Aqua")) %>%
    dplyr::filter(note.type != "") %>% ## This removes notes inserted automatically using BB integrated AI tool
    dplyr::group_by(IDhaul) %>%
    dplyr::mutate(ID3 = rank(haul_number,
                             ties.method = "first")) %>% # ID3 = bycatch "number" (rank) per haul
    dplyr::ungroup() %>%
    dplyr::mutate(IDbc = paste(IDhaul, ID3, sep = ".")) %>%
    dplyr::select(-ID3,-haul_number,-IDhaul,-colour.name,-note.type)
  BBdata <- merge(BBdata, tmp.bc, by = 'IDevent', all.x = TRUE) %>%
    dplyr::arrange(vessel, as.Date(date), IDevent)

  if(incl.fish == TRUE){
    tmp.catch <- BBdata %>%
      dplyr::select(c(haul_number, IDhaul, IDevent, colour.name, note.type)) %>%
      dplyr::filter(colour.name %in% c("Gray","Grey","DarkKhaki","Thistle",
                                       "SaddleBrown")) %>%
      dplyr::filter(note.type != "") %>% ## This removes notes inserted automatically using BB integrated AI tool
      dplyr::group_by(IDhaul) %>%
      dplyr::mutate(ID3 = rank(haul_number,
                               ties.method = "first")) %>% # ID3 = bycatch "number" (rank) per haul
      dplyr::ungroup() %>%
      dplyr::mutate(IDcatch = paste(IDhaul, ID3, sep = ".")) %>%
      dplyr::select(-ID3,-haul_number,-IDhaul,-colour.name,-note.type)
    BBdata <- merge(BBdata, tmp.catch, by = 'IDevent', all.x = TRUE) %>%
      dplyr::arrange(vessel, as.Date(date), IDevent)
  }

  ## Add variable sealmarks #----
  BBdata <- BBdata %>%
    dplyr::mutate(sealmarks = dplyr::if_else(colour.name == "DeepPink", 1, 0)) %>%
    dplyr::group_by(IDhaul) %>%
    dplyr::mutate(sealmarks = ifelse(any(sealmarks == 1), 1, sealmarks)) %>%
    dplyr::ungroup()

  ## Return final dt #----
  return(BBdata)
}
