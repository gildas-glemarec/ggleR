#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @export
logbook_import <- function(x){
  . <- vessel.length <- DFADfvd_ret <- Date <- FD <- IDFD <- d <- eart <- f.mesh <- fid <- fngdato <- hel <- home_harbour <- i.bgrad <- i.lat <- i.lgrad <- i.lon <- i.lplads <- ices.area <- icesrect <- lat <- lat_home <- latin <- lon <- lon_home <- lplads <- m <- maske <- mesh <- metier_level6_ret <- path <-  read.csv <- redskb <- restrict_study_period <- square <- target <- tot.landings <- tot.val.landings <- vrd <- y <- NULL
  `%notin%` <- Negate(`%in%`)
  logbook <- ggleR::load_data(x)

  ## Temporal dummy variables
  logbook$fngdato <- base::as.Date(strptime(logbook$fngdato, "%y%m%d"))
  logbook$m <- lubridate::month(logbook$fngdato)
  logbook$y <- lubridate::year(logbook$fngdato)

  #### What's the period (in years) of the dataset?
  study_period <- c(min(logbook$y):max(logbook$y))
  ## Restrict the study period?
  logbook <- data.table::data.table(logbook)
  if(exists("restrict_study_period")){
    logbook <- logbook[y %in% restrict_study_period]
  } else(logbook <- logbook[y %in% study_period]
  )

  ## Housekeeping
  logbook <- data.table::data.table(logbook %>%
                                      dplyr::filter(redskb %in% c('GN','GND','GNS','GTN','GTR')) %>%
                                      dplyr::mutate(maske = dplyr::na_if(maske,"")) %>%
                                      dplyr::mutate(maske = dplyr::na_if(maske,".")) %>%
                                      ## Filter out rows based on "faulty" landings
                                      dplyr::filter(!latin %in%
                                                      c("Anguilla anguilla", ## Mostly caught in fykes
                                                        # "Osteichthyes", "", ## too vague
                                                        ## Crustacean/Gastropods/Bivalves are not targeted
                                                        ## in the region w. GN:
                                                        "Palaemon serratus","Astacus astacus",
                                                        "Nephrops norvegicus",
                                                        "Pandalus borealis", "Crangon crangon",
                                                        "Buccinum undatum", "Gastropoda",
                                                        "Mytilus edulis","Spisula solida",
                                                        ## Some typical freshwater spp are mentioned:
                                                        # "Abramis brama","Tinca tinca",
                                                        ## Others have no distribution in DK:
                                                        "Boreogadus saida",
                                                        ## Or live in much deeper waters:
                                                        "Coryphaenoides rupestris",
                                                        ## Not fished in gillnets or bycatches
                                                        "Scomber scombrus",
                                                        "Trachurus trachurus",
                                                        "Abramis brama","Acipenser sturio",
                                                        "Alosa fallax","Belone belone",
                                                        "Brama brama","Chimaera monstrosa",
                                                        "Conger conger","Coregonus lavaretus",
                                                        "Esox lucius","Hippoglossoides platessoides",
                                                        "Labrus bergylta","Lamna nasus",
                                                        "Molva dypterygia","Mustelus lenticulatus",
                                                        "Mustelus mustelus","Oncorhynchus mykiss",
                                                        "Osmerus eperlanus","Perca fluviatilis",
                                                        "Phycis blennoides","Prionace glauca",
                                                        "Rajidae","Reinhardtius hippoglossoides",
                                                        "Rutilus rutilus","Salmo salar",
                                                        "Salmo trutta","Scyliorhinus canicula",
                                                        "Sparus aurata","Thunnus thynnus",
                                                        "Zeus faber","Echinus esculentus")) %>%
                                      dplyr::filter(!eart %in% c("Additional Payment")) %>%
                                      ## Quick fix
                                      dplyr::mutate(square = dplyr::if_else(square=='40B2','40G2',square)),
                                    key = 'fid')
  ## Fix negative values of landings and landings value
  logbook$hel <- abs(logbook$hel)
  logbook$vrd <- abs(logbook$vrd)

  ## Flag the country
  logbook$flag <- as.character('DK')

  ## Vessel length
  logbook$vessel.length <- as.numeric(logbook$oal)
  logbook <- logbook %>%
    dplyr::mutate(f.length =
                    dplyr::case_when(vessel.length < 8 ~ "<8m",
                                     dplyr::between(vessel.length, 8, 10) ~ "8-10m",
                                     dplyr::between(vessel.length, 10, 12) ~ "10-12m",
                                     dplyr::between(vessel.length, 12, 15) ~ "12-15m",
                                     vessel.length > 15 ~ ">15m",
                                     TRUE ~ as.character(vessel.length)
                    )
    ) %>%
    mutate(oal = if_else(vessel.length < 15,
                         '<15m',
                         '>15m'))

  ## Eyeballing the mesh size + registered gear + target species,
  ## there are issues. Let's fix the obvious
  logbook$maske <- as.numeric(as.character(logbook$maske))
  logbook[, maske := ifelse(maske>=400,NA,maske)]
  logbook$metier_level6_ret <- as.character(logbook$metier_level6_ret)
  logbook[, metier_level6_ret := ifelse(latin == "Clupea harengus" &
                                          metier_level6_ret != "GNS_SPF_32-109_0_0",
                                        "GNS_SPF_32-109_0_0", metier_level6_ret)]
  ## Some rows have info on metier, but not on mesh. We can assume that they use
  ## they use the minimal mesh size in the category
  # table(logbook[is.na(maske)]$metier_level6_ret)
  logbook[, maske := ifelse(metier_level6_ret == "GNS_SPF_>=220_0_0" & maske < 220 |
                              metier_level6_ret == "GNS_DEF_>=220_0_0" & maske < 220,
                            230,
                            ifelse(metier_level6_ret=="GND_ANA_>=157_0_0" & maske < 157 |
                                     metier_level6_ret=="GNS_ANA_>=157_0_0"& maske < 157 |
                                     metier_level6_ret=="GNS_SPF_>=157_0_0"& maske < 157 |
                                     metier_level6_ret=="GNS_DEF_>=157_0_0"& maske < 157,
                                   157,
                                   ifelse(metier_level6_ret=="GNS_SPF_120-219_0_0"& maske < 120|
                                            metier_level6_ret=="GND_DEF_120-219_0_0"& maske < 120|
                                            metier_level6_ret=="GNS_DEF_120-219_0_0" & maske < 120,
                                          120,
                                          ifelse(metier_level6_ret=="GNS_ANA_110-156_0_0" & maske < 110|
                                                   metier_level6_ret=="GNS_DEF_110-156_0_0"& maske < 110|
                                                   metier_level6_ret=="GNS_SPF_110-156_0_0"& maske < 110,
                                                 110,
                                                 ifelse(metier_level6_ret=="GNS_DEF_100-119_0_0" & maske < 100|
                                                          metier_level6_ret=="GNS_SPF_100-119_0_0" & maske < 100,
                                                        100,
                                                        ifelse(metier_level6_ret=="GNS_DEF_90-109_0_0"& maske<90|
                                                                 metier_level6_ret=="GNS_ANA_90-109_0_0"& maske<90|
                                                                 metier_level6_ret=="GNS_SPF_90-99_0_0" & maske<90|
                                                                 metier_level6_ret=="GNS_DEF_90-99_0_0" & maske<90,
                                                               90,
                                                               ifelse(metier_level6_ret=="GNS_FWS_>0_0_0"& maske<18|
                                                                        metier_level6_ret=='GNS_CRU_>0_0_0'& maske<18,
                                                                      18,
                                                                      ifelse(metier_level6_ret=="GNS_SPF_32-109_0_0"& maske<32,
                                                                             32,
                                                                             ifelse(metier_level6_ret=="GNS_SPF_10-30_0_0" & maske<10,
                                                                                    10,
                                                                                    ifelse(metier_level6_ret=='GND_SPF_50-70_0_0' & maske<50|
                                                                                             metier_level6_ret=='GNS_SPF_50-70_0_0' & maske<50,
                                                                                           50,
                                                                                           maske
                                                                                    ))))))))))]
  logbook[, mesh := ifelse(test = !is.na(maske), yes = maske,
                           ifelse(metier_level6_ret == "GNS_SPF_>=220_0_0" |
                                    metier_level6_ret == "GNS_DEF_>=220_0_0",
                                  230, # Mean value for these metiers
                                  ifelse(metier_level6_ret == "GND_ANA_>=157_0_0" |
                                           metier_level6_ret == "GNS_ANA_>=157_0_0" |
                                           metier_level6_ret == "GNS_SPF_>=157_0_0" |
                                           metier_level6_ret == "GNS_DEF_>=157_0_0" |
                                           metier_level6_ret ==  "GNS_SPF_120-219_0_0" |
                                           metier_level6_ret ==  "GND_DEF_120-219_0_0" |
                                           metier_level6_ret ==  "GNS_DEF_120-219_0_0",
                                         170, # Mean value for these metiers
                                         ifelse(metier_level6_ret == "GNS_ANA_110-156_0_0" |
                                                  metier_level6_ret == "GNS_DEF_110-156_0_0" |
                                                  metier_level6_ret == "GNS_SPF_110-156_0_0",
                                                130, # Mean value for these metiers
                                                ifelse(metier_level6_ret == "GNS_DEF_100-119_0_0"|
                                                         metier_level6_ret == "GNS_SPF_100-119_0_0",
                                                       110, # Mean value for these metiers
                                                       ifelse(metier_level6_ret == "GNS_DEF_90-109_0_0" |
                                                                metier_level6_ret == "GNS_ANA_90-109_0_0" |
                                                                metier_level6_ret == "GNS_SPF_90-99_0_0" |
                                                                metier_level6_ret == "GNS_DEF_90-99_0_0" |
                                                                metier_level6_ret ==  "GNS_FWS_>0_0_0",
                                                              90, # Mean value for these metiers
                                                              ifelse(metier_level6_ret == "GNS_SPF_32-109_0_0",
                                                                     50, # Mean value for this metiers
                                                                     ifelse(metier_level6_ret == "GNS_SPF_10-30_0_0",
                                                                            20, # Mean value for this metiers),
                                                                            ifelse(metier_level6_ret == 'GND_SPF_50-70_0_0' |
                                                                                     metier_level6_ret == 'GNS_SPF_50-70_0_0',
                                                                                   60,
                                                                                   ifelse(metier_level6_ret == 'GNS_CRU_>0_0_0',
                                                                                          160,
                                                                                          as.numeric(NA))
                                                                            )))))))))]
  ## Add mesh as a factor
  logbook[, f.mesh := data.table::fifelse(mesh<120, '<120mm',
                                          data.table::fifelse(mesh>200, '>200mm',
                                                              '120-200mm'))]
  ## Assume that vessels are fishing closest to their home harbour if they do not
  ## indicate fishing location (icesrect) in Danish logbooks
  harbours <- load_data('Q:/dfad/users/ggle/home/WD/dfadudv_gillnet/harbours')
  harbours <- data.table::data.table(harbours, key = 'fid')
  harbours <- merge(unique(logbook, by = 'fid')[, c('fid','oal')],
                    harbours,
                    by = 'fid')
  harbours <- harbours[, c('fid', 'year', 'landing_harbour', 'home_harbour',
                           'oal'
                           # , 'n_trips', 'hel'
  )]
  data.table::setnames(harbours, old = 'home_harbour', new = 'lplads')
  data.table::setkey(harbours, 'lplads')

  ## Assign coordinates to harbour locations
  harbours.locations <- data.table::as.data.table(sf::st_read("H:/Maps/39337/harbours/XYhavn.shp"))
  harbours.locations <- harbours.locations[lplads %in% c(harbours$lplads)]
  data.table::setkey(harbours.locations, 'lplads')

  ## https://stackoverflow.com/questions/34598139/left-join-using-data-table#34600831
  harbours <- harbours[harbours.locations,
                       on = 'lplads',
                       lon := i.lgrad][harbours.locations,
                                       on = 'lplads',
                                       lat:= i.bgrad]

  ## Merge with logbook data
  logbook <- logbook[harbours,
                     on = 'fid',
                     home_harbour := i.lplads][
                       harbours,
                       on = 'fid',
                       lon_home:= i.lon][harbours,
                                         on = 'fid',
                                         lat_home:= i.lat]

  ### Assign a fishing location ('square') if there are none
  logbook[, icesrect := ifelse(square == '99A9',# | square == 'NANANA',
                               yes = mapplots::ices.rect2(lon_home, lat_home),
                               no = square)]

  ## Some problems remain with a few rows. Note that the following removes 33034 rows
  # table(logbook$icesrect)
  logbook <- logbook[icesrect %notin% c('99A9','NANANA', ## No location
                                        '15J3', ## Northern Atlantic
                                        '61F7','63F8', ## Black Sea
                                        '44F4','44F5','44F6', ## ICES IVa
                                        '39G3','37G2','38G2')] ## ICES 24d

  ## Register fishing location as centroid of ICES stat. rect.
  logbook$lon <- mapplots::ices.rect(logbook$icesrect)[,1]
  logbook$lat <- mapplots::ices.rect(logbook$icesrect)[,2]

  ## Calculate depth and distance to shore of the ICES rect centroids
  logbook <- ggleR::get.depth(logbook)

  ## Create an ID for each (unique) fishing day (FD)
  logbook <- logbook %>%
    dplyr::mutate(Date = fngdato) %>%
    tidyr::separate(Date, c("y","m","d")) %>%
    tidyr::unite(col = Date, c(d,m,y), sep = "-")
  data.table::setDT(logbook)
  logbook[, IDFD := paste(fid,Date,sep='.')]
  logbook[, FD := sum(dplyr::n_distinct(fngdato)),
          by = 'match_alle']

  logbook$m <- lubridate::month(logbook$fngdato)
  logbook$y <- lubridate::year(logbook$fngdato)
  logbook[, quarter := data.table::fifelse(m %in% c(1,2,3), 'Q1',
                                           data.table::fifelse(m %in% c(4,5,6), 'Q2',
                                                               data.table::fifelse(m %in% c(7,8,9), 'Q3',
                                                                                   'Q4')))]
  logbook$quarter <- factor(logbook$quarter, levels= c('Q1','Q2','Q3','Q4'))

  ## Assign correct name to ICES area
  logbook[, ices.area := ifelse(DFADfvd_ret == '3AI', 'isefjord',
                                ifelse(DFADfvd_ret == '3AN', '3.a.20',
                                       ifelse(DFADfvd_ret == '3AS', '3.a.21',
                                              ifelse(DFADfvd_ret == '3B', '3.b.23',
                                                     ifelse(DFADfvd_ret == '3C22', '3.c.22',
                                                            ifelse(DFADfvd_ret == '3D24', '3.d.24',
                                                                   ifelse(DFADfvd_ret == '3D25', '3.d.25',
                                                                          ifelse(DFADfvd_ret == '3D26', '3.d.26',
                                                                                 ifelse(DFADfvd_ret == '4B', '4.b',
                                                                                        ifelse(DFADfvd_ret == '4BX', '3.c.22',
                                                                                               ifelse(DFADfvd_ret == '3AI3', 'isefjord',
                                                                                                      'NA')))))))))))]
  ## We have no data from the Baltic Proper, so we need to remove those hauls in
  ## subdivisions 24, 25 and 26.
  logbook <- logbook[ices.area %notin% c('3.d.24','3.d.25','3.d.26',
                                         '3.d.27','3.d.28','3.d.29')]

  ## Main target species (landed) per fishing day
  ## We have thousands of rows with no information on catch weight. We will set
  ## all this to be 0kg (to not discard these rows in the process)
  logbook$vrd <- tidyr::replace_na(logbook$vrd, 0)

  ## Total value of the catch per fishing day
  ## We have thousands of rows with no information on catch value We will set
  ## all this to be 0 (to not discard these rows in the process)
  logbook$vrd <- tidyr::replace_na(logbook$vrd, 0)
  logbook <- logbook[
    , tot.val.landings := sum(vrd),
    by = 'IDFD']
  ## Total weight of the catch per fishing day
  ## We have thousands of rows with no information on catch weight. We will set
  ## all this to be 0kg (to not discard these rows in the process)
  logbook$hel <- tidyr::replace_na(logbook$hel, 0)
  logbook <- logbook[
    , tot.landings := sum(hel),
    by = 'IDFD']

  ### Main target in VALUE landed ##
  ## The following will create 2 new variables (latin and target), which are the
  ## most important catch in terms of landings value per trip
  logbook <- merge(logbook,
                   logbook[logbook[, .I[base::which.max(vrd)],
                                   by = 'IDFD']$V1][, .SD, .SDcols = c('IDFD',
                                                                       'latin')],
                   by = 'IDFD')
  ### Main target in WEIGHT landed ##
  ## The following will create 2 new variables (latin and target), which are the
  ## most important catch in terms of landings weight per trip
  # logbook <- merge(logbook,
  #                  logbook[, .SD[which.max(hel)],
  #                          by = 'IDFD'][, .SD, .SDcols = c('IDFD', 'latin')],
  #                  by = 'IDFD')
  names(logbook)[names(logbook)=="latin.x"] <- "latin"
  names(logbook)[names(logbook)=="latin.y"] <- "target"

  ## If lumpsucker is landed, then we assume that lumpsucker is the main target
  ## species for that fishing day
  logbook[, target := lapply(.SD, function(x) if(base::any(target == 'Cyclopterus lumpus'))
    'Cyclopterus lumpus' else target), by = .(IDFD)]
}
