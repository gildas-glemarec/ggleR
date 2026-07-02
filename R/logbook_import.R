#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @param incl.all.ices.areas incl. fishing effort data from the Baltic Proper?
#' @param path.to.raster path to the directory where the depth raster is
#' @param path_to_harbour_list path to the directory where the list of vessels per harbour per year is located
#' @param path_to_harbour_shp path to the directory where the harbours' shapefile is located
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @export
logbook_import <- function(x,
                           incl.all.ices.areas = FALSE,
                           path.to.raster = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/GIS/D5_2020.tif",
                           path_to_harbour_list = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/harbours/by.year",
                           path_to_harbour_shp = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/harbours/XYhavn.shp"){

  square_ret <- oal <- f.length <- vessel.length.split15 <- mostICESrect2 <- geom <- . <- landing_harbour <- mostICESrect <- square2 <- newID <- dfadfvd_ret <- lgrad <- bgrad <- quarter <- vessel.length <- DFADfvd_ret <- Date <- FD <- IDFD <- d <- eart <- f.mesh <- fid <- fngdato <- hel <- home_harbour <- i.bgrad <- i.lat <- i.lgrad <- i.lon <- i.lplads <- ices.area <- icesrect <- lat <- lat_home <- latin <- lon <- lon_home <- lplads <- m <- maske <- mesh <- metier_level6_ret <- metier_level_6_new <- path <-  read.csv <- redskb <- restrict_study_period <- square <- target <- tot.landings <- tot.val.landings <- vrd <- y <- NULL

  `%notin%` <- Negate(`%in%`)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  logbook <- load_data(x)

  ## Temporal dummy variables
  logbook$fngdato <- base::as.Date(strptime(logbook$fngdato, "%y%m%d"))
  logbook$m <- lubridate::month(logbook$fngdato)
  logbook$y <- lubridate::year(logbook$fngdato)

  #### What's the period (in years) of the dataset?
  study_period <- c(min(logbook$y):max(logbook$y))

  logbook <- data.table::data.table(logbook)

  ## Housekeeping
  ## ## Update June 2026: limit filtering to L6new and species landed
  logbook <- data.table::data.table(
    logbook %>%
      dplyr::filter(stringr::str_starts(metier_level_6_new, "GN")) %>%
      dplyr::mutate(maske = dplyr::na_if(maske,"")) %>%
      dplyr::mutate(maske = dplyr::na_if(maske,".")) %>%
      ## Filter out rows based on "faulty" landings
      dplyr::filter(!latin %in% c(
        ## Crustacean/Gastropods/Bivalves are not targeted
        ## in the region w. GN:
        "Palaemon serratus",
        "Astacus astacus",
        "Nephrops norvegicus",
        "Pandalus borealis",
        "Crangon crangon",
        "Buccinum undatum",
        "Gastropoda",
        "Mytilus edulis",
        "Spisula solida") ) %>%
      dplyr::filter(!eart %in% c("Additional Payment")) %>%
      ## Quick fix
      dplyr::mutate(square_ret = dplyr::if_else(square=='40B2',
                                                '40G2',
                                                square_ret)),
    key = 'fid')

  ## Assign correct name to ICES area
  logbook[, ices.area := data.table::fcase(
    dfadfvd_ret == '3AI' | dfadfvd_ret == '3AI3', 'isefjord',
    dfadfvd_ret == '3AN', '3.a.20',
    dfadfvd_ret == '3AS', '3.a.21',
    dfadfvd_ret == '3B', '3.b.23',
    dfadfvd_ret == '3C22' | dfadfvd_ret == '4BX', '3.c.22',
    dfadfvd_ret == '3D24', '3.d.24',
    dfadfvd_ret == '3D25', '3.d.25',
    dfadfvd_ret == '3D26', '3.d.26',
    dfadfvd_ret == '4A', '4.a',
    dfadfvd_ret == '4B', '4.b',
    dfadfvd_ret == '4C', '4.c',
    dfadfvd_ret == '4L', 'limfjord',
    dfadfvd_ret == '4R', 'ringk.fjord',
    dfadfvd_ret == '4N', 'nissum.fjord',
    default = NA_character_
  )]

  if(incl.all.ices.areas == FALSE){
    ## We have no data from the Baltic Proper, so we need to remove those hauls in
    ## subdivisions 24, 25 and 26.
    logbook <- logbook[ices.area %notin% c('3.d.24','3.d.25','3.d.26',
                                           '3.d.27','3.d.28','3.d.29')]
  }

  # ## Fix negative values of landings and landings value
  # logbook$hel <- abs(logbook$hel)
  # logbook$vrd <- abs(logbook$vrd)

  ## Flag the country
  logbook$flag <- as.character('DK')

  ## Vessel length
  logbook[, oal := data.table::fcase(
    is.na(oal) | oal == '.' & fid == 'AS191', "8.94",
    is.na(oal) | oal == '.' & fid == 'L39', "8.62",
    is.na(oal) | oal == '.' & fid == 'SG237', "9", ## this one is made up
    default = oal
  )]
  logbook$vessel.length <- as.numeric(logbook$oal)
  logbook[, f.length := data.table::fcase(
    (vessel.length < 8), "<8m",
    (dplyr::between(vessel.length, 8, 10)), "8-10m",
    (dplyr::between(vessel.length, 10, 12)), "10-12m",
    (dplyr::between(vessel.length, 12, 15)), "12-15m",
    (vessel.length > 15), ">15m",
    default = as.character(vessel.length)
  )]
  logbook[, vessel.length.split15 := data.table::fifelse(vessel.length < 15,
                                                         '<15m', '>15m')]

  ## Eyeballing the mesh size + registered gear + target species,
  ## there are issues. Let's fix the obvious
  logbook$maske <- as.numeric(as.character(logbook$maske))
  logbook[, maske := data.table::fifelse(maske>=400, NA, maske)]
  ## Some rows have info on metier, but not on mesh. We assume that they use
  ## they use the minimal mesh size in the category
  # table(logbook[is.na(maske)]$metier_level_6_new, useNA = 'always')
  logbook[, maske := data.table::fcase(
    # 230
    (metier_level_6_new == "GNS_SPF_>=220_0_0" &
       maske < 220) |
      (metier_level_6_new == "GNS_DEF_>=220_0_0" &
         maske < 220), 230,
    # 157
    (metier_level_6_new %in% c("GND_ANA_>=157_0_0", "GNS_ANA_>=157_0_0",
                               "GNS_SPF_>=157_0_0", "GNS_DEF_>=157_0_0") &
       maske < 157), 157,
    # 120
    (metier_level_6_new %in% c("GNS_SPF_120-219_0_0", "GND_DEF_120-219_0_0",
                               "GNS_DEF_120-219_0_0") &
       maske < 120), 120,
    # 110
    (metier_level_6_new %in% c("GNS_ANA_110-156_0_0", "GNS_DEF_110-156_0_0",
                               "GNS_SPF_110-156_0_0") & maske < 110), 110,
    # 100
    (metier_level_6_new %in% c("GNS_DEF_100-119_0_0", "GNS_SPF_100-119_0_0") &
       maske < 100), 100,
    # 90
    (metier_level_6_new %in% c("GNS_DEF_90-109_0_0", "GNS_ANA_90-109_0_0",
                               "GNS_SPF_90-99_0_0", "GNS_DEF_90-99_0_0") &
       maske < 90), 90,
    # 18
    (metier_level_6_new %in% c("GNS_FWS_>0_0_0", "GNS_CRU_>0_0_0") &
       maske < 18), 18,
    # 32
    (metier_level_6_new == "GNS_SPF_32-109_0_0" &
       maske < 32), 32,
    # 10
    (metier_level_6_new == "GNS_SPF_10-30_0_0" &
       maske < 10), 10,
    # 50
    (metier_level_6_new %in% c("GND_SPF_50-70_0_0", "GNS_SPF_50-70_0_0",
                               "GNS_DEF_50-70_0_0") &
       maske < 50), 50,
    # Default: keep original maske
    default = maske
  )]
  # table(logbook$maske, useNA = 'always')
  logbook[, mesh := data.table::fcase(
    !is.na(maske), maske,
    # 230
    metier_level_6_new %in% c("GNS_SPF_>=220_0_0", "GNS_DEF_>=220_0_0", "GNS_CRU_>=220_0_0"), 230,
    # 170
    metier_level_6_new %in% c(
      "GND_ANA_>=157_0_0", "GNS_ANA_>=157_0_0", "GNS_SPF_>=157_0_0", "GNS_DEF_>=157_0_0",
      "GNS_SPF_120-219_0_0", "GND_DEF_120-219_0_0", "GNS_DEF_120-219_0_0", "GNS_CRU_120-219_0_0"
    ), 170,
    # 130
    metier_level_6_new %in% c("GNS_ANA_110-156_0_0", "GNS_DEF_110-156_0_0", "GNS_SPF_110-156_0_0"), 130,
    # 110
    metier_level_6_new %in% c("GNS_DEF_100-119_0_0", "GNS_SPF_100-119_0_0", "GNS_CRU_100-119_0_0"), 110,
    # 90
    metier_level_6_new %in% c(
      "GNS_DEF_90-109_0_0", "GNS_ANA_90-109_0_0", "GNS_SPF_90-99_0_0",
      "GNS_DEF_90-99_0_0", "GNS_CRU_90-99_0_0", "GNS_FWS_>0_0_0", "GNS_SPF_>0_0_0"
    ), 90,
    # 50
    metier_level_6_new %in% c("GNS_SPF_32-109_0_0", "GNS_DEF_32-89_0_0", "GNS_SPF_32-89_0_0", "GNS_DEF_50-70_0_0"), 50,
    # 20
    metier_level_6_new %in% c(
      "GNS_SPF_10-30_0_0", "GNS_ANA_>0_0_0", "GNS_CAT_>0_0_0",
      "GNS_CRU_10-30_0_0", "GNS_SPF_16-31_0_0"
    ), 20,
    # 60
    metier_level_6_new %in% c(
      "GND_SPF_50-70_0_0", "GNS_CRU_50-70_0_0", "GNS_SPF_50-70_0_0",
      "GNS_DEF_50-70_0_0", "GNS_SPF_>0_0_0"
    ), 60,
    # 160
    metier_level_6_new == "GNS_CRU_>0_0_0", 160,
    # 40
    metier_level_6_new %in% c("GNS_CRU_31-49_0_0", "GNS_DEF_31-49_0_0", "GNS_SPF_31-49_0_0"), 40,
    # 80
    metier_level_6_new %in% c("GNS_CRU_71-89_0_0", "GNS_DEF_71-89_0_0"), 80,
    default = as.numeric(NA)
  )]
  # table(logbook$mesh, useNA = 'always')
  ## Add mesh as a factor
  logbook[, f.mesh := data.table::fcase(mesh<120, '<120mm',
                                        mesh>199, '>199mm',
                                        default = '120-199mm')]

  ## Assume that vessels are fishing closest to their home harbour if they do not
  ## indicate fishing location (icesrect) in Danish logbooks
  harbours <- ggleR::load_data(path_to_harbour_list)
  harbours <- data.table::data.table(harbours, key = 'fid')
  harbours <- unique(logbook, by = 'fid')[
    , c('fid','vessel.length.split15')][harbours, on = 'fid']
  harbours <- harbours[, c('fid', 'year', 'landing_harbour', 'home_harbour',
                           'vessel.length.split15'
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
  logbook$fid.year <- paste(logbook$fid,
                            ifelse( logbook$y > max(harbours$y),
                                    as.character(max(harbours$y)),
                                    as.character(logbook$y) ),
                            sep = '.')
  ## Merge logbook with harbours to add home_harbour, lon_home, and lat_home
  logbook <- merge(logbook,
                   subset(harbours,
                          select = c('fid.year','lon','lat','lplads')),
                   by = "fid.year",
                   all.x = TRUE)
  ## For debugging:
  # View(logbook[is.na(lplads) & square_ret == "NONE"])

  data.table::setnames(logbook, old = c('lon','lat','lplads'),
                       new = c('lon_home','lat_home','home_harbour'))

  ### Assign a fishing location ('icesrect') if there are none
  ### 1. Most frequent ICES rectangle from the same period (here: same month)?
  logbook[, square2 := data.table::fifelse(square_ret %notin% 'NONE', square_ret, NA_character_)]
  logbook[, mostICESrect := Mode(square2), by = c('fid', 'm')]
  logbook[, mostICESrect2 := Mode(square2), by = c('fid')]
  logbook[, mostICESrect := data.table::fifelse(is.na(mostICESrect),
                                                mostICESrect2,
                                                mostICESrect)]
  logbook[, icesrect := data.table::fifelse(square_ret == 'NONE' & !is.na(mostICESrect),
                                            yes = mostICESrect,
                                            no = square_ret)]
  ### 2. If there is no info on location of the effort, then use
  ###    the harbour location as a proxy
  logbook[, icesrect := data.table::fifelse( icesrect == 'NONE' ,
                                             yes = mapplots::ices.rect2(lon_home, lat_home),
                                             no = icesrect)]
  ### 3. If we still have no info on location of the effort (because we don't
  ### the home harbour), then use the value of "dfadfvd_ret"
  logbook[, icesrect := data.table::fcase( is.na(icesrect) & dfadfvd_ret == '3AI', '40G1',
                                           is.na(icesrect) & dfadfvd_ret == '4L', '42F9',
                                           is.na(icesrect) & dfadfvd_ret == '3C22', '39G0',
                                           is.na(icesrect) & dfadfvd_ret == '3AN', '43F8'
  )]
  ## Register fishing location as centroid of ICES stat. rect.
  ices.rectangles <- readRDS('Q:/10-forskningsprojekter/faste-cctv-monitoring/data/GIS/ICES_rect.RDS')
  ices.rectangles$icesrect <- ices.rectangles$ICESNAME
  logbook <- merge(logbook,
                   subset(ices.rectangles,
                          select = c('icesrect','d2shore','depth')),
                   by = "icesrect",
                   all.x = TRUE)
  logbook <- logbook[!is.na(fid)]

  ## Create an ID for each (unique) fishing day (FD)
  logbook <- logbook %>%
    dplyr::mutate(Date = fngdato) %>%
    tidyr::separate(Date, c("y","m","d")) %>%
    tidyr::unite(col = Date, c(d,m,y), sep = "-")
  data.table::setDT(logbook)
  logbook[, IDFD := paste(fid, Date, sep='.')]
  logbook[, FD := sum(dplyr::n_distinct(fngdato)),
          by = 'match_alle']
  logbook[, m := lubridate::month(fngdato)]
  logbook[, y := lubridate::year(fngdato)]
  logbook[, quarter := data.table::fcase(m %in% c(1,2,3), 'Q1',
                                         m %in% c(4,5,6), 'Q2',
                                         m %in% c(7,8,9), 'Q3',
                                         m %in% c(10,11,12),'Q4',
                                         default = NA_character_)]
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
                                     by = 'IDFD']$V1][
                                       , .SD, .SDcols = c('IDFD',
                                                          'latin')],
                     on = c('IDFD')]
  names(logbook)[names(logbook)=="i.latin"] <- "target"

  ## If lumpsucker is landed, then we assume that lumpsucker is the main target
  ## species for that fishing day - Good for now, but in the future, we should
  ## have something like "if lumpsucker is landed and above 20kg, then we assume
  ## that lumpsucker is the main target species for that fishing day"
  logbook[, target := lapply(.SD, function(x) if(base::any(target == 'Cyclopterus lumpus'))
    'Cyclopterus lumpus' else target), by = .(IDFD)]

  ## Clean up the remains of the sf additions:
  logbook[, geom := NULL]

  ## Fix the encoding of Danish characters
  logbook$fid.year  <- iconv(logbook$fid.year ,
                             from = "", to = "UTF-8", sub = "")
  logbook$fid <- iconv(logbook$fid,
                       from = "", to = "UTF-8", sub = "")
  logbook$match_alle <- iconv(logbook$match_alle,
                              from = "", to = "UTF-8", sub = "")
  logbook$dkart <- iconv(logbook$dkart,
                         from = "", to = "UTF-8", sub = "")
  logbook$IDFD <- iconv(logbook$IDFD,
                        from = "", to = "UTF-8", sub = "")


  return(logbook)
}

