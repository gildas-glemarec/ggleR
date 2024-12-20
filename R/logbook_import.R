#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @param path.to.raster path to the directory where the depth raster is
#' @param path_to_harbour_list path to the directory where the list of vessels per harbour per year is located
#' @param path_to_harbour_shp path to the directory where the harbours' shapefile is located
#' @param restrict_study_period A vector of years - e.g., c(2010:2020) - default is NULL
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @export
logbook_import_fast <- function(x,
                                path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/D5_2020.tif",
                                path_to_harbour_list = "Q:/scientific-projects/cctv-monitoring/data/harbours/by.year",
                                path_to_harbour_shp = "Q:/scientific-projects/cctv-monitoring/data/harbours/XYhavn.shp"){
  . <- quarter <- vessel.length <- DFADfvd_ret <- Date <- FD <- IDFD <- d <- eart <- f.mesh <- fid <- fngdato <- hel <- home_harbour <- i.bgrad <- i.lat <- i.lgrad <- i.lon <- i.lplads <- ices.area <- icesrect <- lat <- lat_home <- latin <- lon <- lon_home <- lplads <- m <- maske <- mesh <- metier_level6_ret <- metier_level_6_new <- path <-  read.csv <- redskb <- restrict_study_period <- square <- target <- tot.landings <- tot.val.landings <- vrd <- y <- NULL
  `%notin%` <- Negate(`%in%`)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  logbook <- ggleR::load_data(x)

  ## Temporal dummy variables
  logbook$fngdato <- base::as.Date(strptime(logbook$fngdato, "%y%m%d"))
  logbook$m <- lubridate::month(logbook$fngdato)
  logbook$y <- lubridate::year(logbook$fngdato)

  #### What's the period (in years) of the dataset?
  study_period <- c(min(logbook$y):max(logbook$y))

  logbook <- data.table::data.table(logbook)

  ## Housekeeping
  logbook <- data.table::data.table(logbook %>%
                                      dplyr::filter(redskb %in% c('GN','GND','GNS','GTN','GTR')) %>%
                                      dplyr::filter(stringr::str_starts(metier_level_6_new, "GN")) %>%
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

  ## Assign correct name to ICES area
  logbook[, ices.area := ifelse(dfadfvd_ret == '3AI', 'isefjord',
                                ifelse(dfadfvd_ret == '3AN', '3.a.20',
                                       ifelse(dfadfvd_ret == '3AS', '3.a.21',
                                              ifelse(dfadfvd_ret == '3B', '3.b.23',
                                                     ifelse(dfadfvd_ret == '3C22', '3.c.22',
                                                            ifelse(dfadfvd_ret == '3D24', '3.d.24',
                                                                   ifelse(dfadfvd_ret == '3D25', '3.d.25',
                                                                          ifelse(dfadfvd_ret == '3D26', '3.d.26',
                                                                                 ifelse(dfadfvd_ret == '4A', '4.a',
                                                                                        ifelse(dfadfvd_ret == '4B', '4.b',
                                                                                               ifelse(dfadfvd_ret == '4BX', '3.c.22',
                                                                                                      ifelse(dfadfvd_ret == '4C', '4.c',
                                                                                                             ifelse(dfadfvd_ret == '3AI3', 'isefjord',
                                                                                                                    ifelse(dfadfvd_ret == '4L', 'limfjord',
                                                                                                                           ifelse(dfadfvd_ret == '4R', 'ringk.fjord',
                                                                                                                                  ifelse(dfadfvd_ret == '4N', 'nissum.fjord',
                                                                                                                                         'NA'))))))))))))))))]
  ## We have no data from the Baltic Proper, so we need to remove those hauls in
  ## subdivisions 24, 25 and 26.
  logbook <- logbook[ices.area %notin% c('3.d.24','3.d.25','3.d.26',
                                         '3.d.27','3.d.28','3.d.29')]

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
    dplyr::mutate(oal = if_else(vessel.length < 15,
                                '<15m',
                                '>15m'))
  logbook <- data.table::as.data.table(logbook)

  ## Eyeballing the mesh size + registered gear + target species,
  ## there are issues. Let's fix the obvious
  logbook$maske <- as.numeric(as.character(logbook$maske))
  logbook[, maske := ifelse(maske>=400, NA, maske)]
  # logbook$metier_level_6_new <- as.character(logbook$metier_level_6_new)
  # logbook[, metier_level6_ret := ifelse(latin == "Clupea harengus" &
  #                                         metier_level6_ret != "GNS_SPF_32-109_0_0",
  #                                       "GNS_SPF_32-109_0_0", metier_level6_ret)]
  ## Some rows have info on metier, but not on mesh. We can assume that they use
  ## they use the minimal mesh size in the category
  # table(logbook[is.na(maske)]$metier_level_6_new)
  logbook[, maske := ifelse(metier_level_6_new == "GNS_SPF_>=220_0_0" & maske < 220 |
                              metier_level_6_new == "GNS_DEF_>=220_0_0" & maske < 220,
                            230,
                            ifelse(metier_level_6_new=="GND_ANA_>=157_0_0" & maske < 157 |
                                     metier_level_6_new=="GNS_ANA_>=157_0_0" & maske < 157 |
                                     metier_level_6_new=="GNS_SPF_>=157_0_0" & maske < 157 |
                                     metier_level_6_new=="GNS_DEF_>=157_0_0" & maske < 157,
                                   157,
                                   ifelse(metier_level_6_new=="GNS_SPF_120-219_0_0"& maske < 120|
                                            metier_level_6_new=="GND_DEF_120-219_0_0"& maske < 120|
                                            metier_level_6_new=="GNS_DEF_120-219_0_0" & maske < 120,
                                          120,
                                          ifelse(metier_level_6_new=="GNS_ANA_110-156_0_0" & maske < 110|
                                                   metier_level_6_new=="GNS_DEF_110-156_0_0"& maske < 110|
                                                   metier_level_6_new=="GNS_SPF_110-156_0_0"& maske < 110,
                                                 110,
                                                 ifelse(metier_level_6_new=="GNS_DEF_100-119_0_0" & maske < 100|
                                                          metier_level_6_new=="GNS_SPF_100-119_0_0" & maske < 100,
                                                        100,
                                                        ifelse(metier_level_6_new=="GNS_DEF_90-109_0_0"& maske<90|
                                                                 metier_level_6_new=="GNS_ANA_90-109_0_0"& maske<90|
                                                                 metier_level_6_new=="GNS_SPF_90-99_0_0" & maske<90|
                                                                 metier_level_6_new=="GNS_DEF_90-99_0_0" & maske<90,
                                                               90,
                                                               ifelse(metier_level_6_new=="GNS_FWS_>0_0_0"& maske<18|
                                                                        metier_level_6_new=='GNS_CRU_>0_0_0'& maske<18,
                                                                      18,
                                                                      ifelse(metier_level_6_new=="GNS_SPF_32-109_0_0"& maske<32,
                                                                             32,
                                                                             ifelse(metier_level_6_new=="GNS_SPF_10-30_0_0" & maske<10,
                                                                                    10,
                                                                                    ifelse(metier_level_6_new=='GND_SPF_50-70_0_0' & maske<50|
                                                                                             metier_level_6_new=='GNS_SPF_50-70_0_0' & maske<50|
                                                                                             metier_level_6_new=='GNS_DEF_50-70_0_0' & maske<50,
                                                                                           50,
                                                                                           maske
                                                                                    ))))))))))]
  logbook[, mesh := ifelse(test = !is.na(maske), yes = maske,
                           ifelse(metier_level_6_new == "GNS_SPF_>=220_0_0" |
                                    metier_level_6_new == "GNS_DEF_>=220_0_0" |
                                    metier_level_6_new == "GNS_CRU_>=220_0_0",
                                  230, # Mean value for these metiers
                                  ifelse(metier_level_6_new == "GND_ANA_>=157_0_0" |
                                           metier_level_6_new == "GNS_ANA_>=157_0_0" |
                                           metier_level_6_new == "GNS_SPF_>=157_0_0" |
                                           metier_level_6_new == "GNS_DEF_>=157_0_0" |
                                           metier_level_6_new ==  "GNS_SPF_120-219_0_0" |
                                           metier_level_6_new ==  "GND_DEF_120-219_0_0" |
                                           metier_level_6_new ==  "GNS_DEF_120-219_0_0" |
                                           metier_level_6_new ==  "GNS_CRU_120-219_0_0",
                                         170, # Mean value for these metiers
                                         ifelse(metier_level_6_new == "GNS_ANA_110-156_0_0" |
                                                  metier_level_6_new == "GNS_DEF_110-156_0_0" |
                                                  metier_level_6_new == "GNS_SPF_110-156_0_0",
                                                130, # Mean value for these metiers
                                                ifelse(metier_level_6_new == "GNS_DEF_100-119_0_0" |
                                                         metier_level_6_new == "GNS_SPF_100-119_0_0" |
                                                         metier_level_6_new == "GNS_CRU_100-119_0_0",
                                                       110, # Mean value for these metiers
                                                       ifelse(metier_level_6_new == "GNS_DEF_90-109_0_0" |
                                                                metier_level_6_new == "GNS_ANA_90-109_0_0" |
                                                                metier_level_6_new == "GNS_SPF_90-99_0_0" |
                                                                metier_level_6_new == "GNS_DEF_90-99_0_0" |
                                                                metier_level_6_new == "GNS_CRU_90-99_0_0" |
                                                                metier_level_6_new == "GNS_FWS_>0_0_0"|
                                                                metier_level_6_new == 'GNS_SPF_>0_0_0',
                                                              90, # Mean value for these metiers
                                                              ifelse(metier_level_6_new == "GNS_SPF_32-109_0_0" |
                                                                       metier_level_6_new == "GNS_DEF_32-89_0_0" |
                                                                       metier_level_6_new == "GNS_SPF_32-89_0_0" |
                                                                       metier_level_6_new == "GNS_DEF_50-70_0_0",
                                                                     50, # Mean value for this metiers
                                                                     ifelse(metier_level_6_new == "GNS_SPF_10-30_0_0" |
                                                                              metier_level_6_new == "GNS_ANA_>0_0_0" |
                                                                              metier_level_6_new == "GNS_CAT_>0_0_0" |
                                                                              metier_level_6_new == "GNS_CRU_10-30_0_0" |
                                                                              metier_level_6_new == "GNS_SPF_16-31_0_0",
                                                                            20, # Mean value for this metiers),
                                                                            ifelse(metier_level_6_new == 'GND_SPF_50-70_0_0' |
                                                                                     metier_level_6_new == 'GNS_CRU_50-70_0_0' |
                                                                                     metier_level_6_new == 'GNS_SPF_50-70_0_0' |
                                                                                     metier_level_6_new == 'GNS_DEF_50-70_0_0' |
                                                                                     metier_level_6_new == 'GNS_SPF_>0_0_0',
                                                                                   60,
                                                                                   ifelse(metier_level_6_new == 'GNS_CRU_>0_0_0',
                                                                                          160,
                                                                                          ifelse(metier_level_6_new == 'GNS_CRU_31-49_0_0' |
                                                                                                   metier_level_6_new == 'GNS_DEF_31-49_0_0' |
                                                                                                   metier_level_6_new == 'GNS_SPF_31-49_0_0',
                                                                                                 40,
                                                                                                 ifelse(metier_level_6_new == 'GNS_CRU_71-89_0_0' |
                                                                                                          metier_level_6_new == 'GNS_DEF_71-89_0_0' |
                                                                                                          metier_level_6_new == 'GNS_DEF_71-89_0_0',
                                                                                                        80,
                                                                                                        as.numeric(NA))
                                                                                          )))))))))))]
  ## Add mesh as a factor
  logbook[, f.mesh := data.table::fifelse(mesh<120, '<120mm',
                                          data.table::fifelse(mesh>200, '>200mm',
                                                              '120-200mm'))]
  ## Assume that vessels are fishing closest to their home harbour if they do not
  ## indicate fishing location (icesrect) in Danish logbooks
  harbours <- ggleR::load_data(path_to_harbour_list)
  harbours <- data.table::data.table(harbours, key = 'fid')
  harbours <- unique(logbook, by = 'fid')[, c('fid','oal')][harbours,
                                                            on = 'fid']
  # harbours <- data.table::merge(unique(logbook, by = 'fid')[, c('fid','oal')],
  #                   harbours,
  #                   by = 'fid')
  harbours <- harbours[, c('fid', 'year', 'landing_harbour', 'home_harbour',
                           'oal'
                           # , 'n_trips', 'hel'
  )]
  data.table::setnames(harbours, old = 'home_harbour', new = 'lplads')
  data.table::setkey(harbours, 'lplads')

  ## Assign coordinates to harbour locations
  harbours.locations <- data.table::as.data.table(sf::st_read(path_to_harbour_shp))
  harbours.locations <- harbours.locations[lplads %in% c(harbours$lplads)]
  data.table::setkey(harbours.locations, 'lplads')
  harbours[, lplads := data.table::fifelse(lplads == '', Mode(lplads), lplads),
           by = c('fid')]
  harbours[, lplads := data.table::fifelse(lplads == '', landing_harbour, lplads)]
  harbours <- data.table::copy(harbours)[harbours.locations,
                                         on = 'lplads',
                                         `:=`(lon = lgrad,
                                              lat = bgrad)]
  ## Merge with logbook data; fid.year because the same vessel (name) can change
  ## owner (and thus also home harbour) from one year to the next
  harbours$fid.year <- paste(harbours$fid, harbours$year, sep='.')
  harbours <- unique(harbours, by = "fid.year")
  logbook$fid.year <- paste(logbook$fid, as.character(logbook$y), sep='.')

  # Merge logbook with harbours to add home_harbour, lon_home, and lat_home
  logbook <- logbook[subset(harbours,
                            select = c('fid.year','lon','lat','lplads')),
                     on = 'fid.year']
  # logbook <- data.table::merge(logbook,
  #                  subset(harbours,
  #                         select = c('fid.year','lon','lat','lplads')),
  #                  by = c('fid.year') )
  data.table::setnames(logbook, old = c('lon','lat','lplads'),
                       new = c('lon_home','lat_home','home_harbour'))

  ### Assign a fishing location ('icesrect') if there are none
  ### 1. Most frequent ICES rectangle from the same period (here: same month)?
  logbook[, newID := paste(fid, m, sep = '_')]
  logbook[, square2 := data.table::fifelse(square %notin% '99A9', square, NA_character_)]
  logbook[, mostICESrect := Mode(square2), by = c('newID')]
  logbook[, icesrect := data.table::fifelse(square == '99A9' & !is.na(mostICESrect),
                                            yes = mostICESrect,
                                            no = square)]
  ### 2. If there is no info on location of the effort, then use
  ###    the harbour location as a proxy
  logbook[, icesrect := data.table::fifelse(square == '' | is.na(square) | icesrect == '99A9' | is.na(icesrect),
                                            yes = mapplots::ices.rect2(lon_home, lat_home),
                                            no = icesrect)]

  ## Register fishing location as centroid of ICES stat. rect.
  # logbook[, lon := mapplots::ices.rect(logbook$icesrect)[,1]]
  # logbook[, lat := mapplots::ices.rect(logbook$icesrect)[,2]]
  #
  ## Calculate depth and distance to shore of the ICES rect centroids
  # get.depth <- function(x,
  #                       path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/alldepth.tif"){
  #   ## Depth at point
  #   depth.ras.dk <- terra::rast(x = path.to.raster)
  #   x <- data.table::as.data.table(x)
  #   dk.sfpts <- sf::st_as_sf(x, coords = c('lon','lat'), na.fail = FALSE)
  #   depth.dk.df <- (terra::extract(x = depth.ras.dk,
  #                                  y = dk.sfpts,
  #                                  df = TRUE))$alldepth
  #   x <- data.table::data.table(x)[, depth:= depth.dk.df]
  #   x <- x[, depth := data.table::fifelse(depth>0, -2, depth)]
  #   return(x)
  #   gc()
  # }
  # logbook <- get.depth(logbook,
  #                      path.to.raster = 'Q:/scientific-projects/cctv-monitoring/data/GIS/alldepth.tif')
  ices.rectangles <- readRDS('Q:/scientific-projects/cctv-monitoring/data/GIS/ICES_rect.RDS')
  # ices.rectangles <- sf::read_sf('H:/c-users/Maps/DepthDK/ICES rect depth.gpkg')
  ices.rectangles$icesrect <- ices.rectangles$ICESNAME
  logbook <- logbook[subset(ices.rectangles,
                            select = c('icesrect','d2shore','depth')),
                     on = c('icesrect')]
  # logbook <- data.table::merge(logbook,
  #                  subset(ices.rectangles, select = c('ICESNAME','d2shore','depth')),
  #                  by.x = 'icesrect', by.y = 'ICESNAME', all.x = TRUE)

  ## Create an ID for each (unique) fishing day (FD)
  logbook <- logbook %>%
    dplyr::mutate(Date = fngdato) %>%
    tidyr::separate(Date, c("y","m","d")) %>%
    tidyr::unite(col = Date, c(d,m,y), sep = "-")
  data.table::setDT(logbook)
  logbook[, IDFD := paste(fid, Date,sep='.')]
  logbook[, FD := sum(dplyr::n_distinct(fngdato)),
          by = 'match_alle']

  logbook$m <- lubridate::month(logbook$fngdato)
  logbook$y <- lubridate::year(logbook$fngdato)
  logbook[, quarter := data.table::fifelse(m %in% c(1,2,3), 'Q1',
                                           data.table::fifelse(m %in% c(4,5,6), 'Q2',
                                                               data.table::fifelse(m %in% c(7,8,9), 'Q3',
                                                                                   'Q4')))]
  logbook$quarter <- factor(logbook$quarter, levels= c('Q1','Q2','Q3','Q4'))

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
  logbook <- logbook[logbook[logbook[, .I[base::which.max(vrd)],
                                     by = 'IDFD']$V1][, .SD, .SDcols = c('IDFD',
                                                                         'latin')],
                     on = c('IDFD')]
  # logbook <- merge(logbook,
  #                  logbook[logbook[, .I[base::which.max(vrd)],
  #                                  by = 'IDFD']$V1][, .SD, .SDcols = c('IDFD',
  #                                                                      'latin')],
  #                  by = 'IDFD')

  ### Main target in WEIGHT landed ##
  ## The following will create 2 new variables (latin and target), which are the
  ## most important catch in terms of landings weight per trip
  # logbook <- data.table::merge(logbook,
  #                  logbook[, .SD[which.max(hel)],
  #                          by = 'IDFD'][, .SD, .SDcols = c('IDFD', 'latin')],
  #                  by = 'IDFD')
  names(logbook)[names(logbook)=="latin.x"] <- "latin"
  names(logbook)[names(logbook)=="latin.y"] <- "target"

  ## If lumpsucker is landed, then we assume that lumpsucker is the main target
  ## species for that fishing day
  logbook[, target := lapply(.SD, function(x) if(base::any(target == 'Cyclopterus lumpus'))
    'Cyclopterus lumpus' else target), by = .(IDFD)]

  return(logbook)
}

