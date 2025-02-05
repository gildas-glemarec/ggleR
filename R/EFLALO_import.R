#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @param path.to.raster path to the directory where the depth raster is
#' @param path_to_harbour_list path to the directory where the list of vessels per harbour per year is located
#' @param path_to_harbour_shp path to the directory where the harbours' shapefile is located
#' @param study_period A vector of years - e.g., c(2010:2020) - default is NULL
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @export
logbook_import <- function(x, #x <- 'Q:/dfad/users/ggle/data/EFLALO'
                           study_period = NULL,
                           # path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/D5_2020.tif",
                           # path_to_harbour_list = "Q:/scientific-projects/cctv-monitoring/data/harbours/by.year",
                           # path_to_harbour_shp = "Q:/scientific-projects/cctv-monitoring/data/harbours/XYhavn.shp"
){
  . <- quarter <- vessel.length <- LE_DIV <- Date <- FD <- IDFD <- d <- eart <- f.mesh <- VE_REF <- fngdato <- hel <- home_harbour <- i.bgrad <- i.lat <- i.lgrad <- i.lon <- i.lplads <- ices.area <- icesrect <- lat <- lat_home <- latin <- lon <- lon_home <- lplads <- m <- LE_MSZ <- mesh <- metier_level6_ret <- LE_MET <- path <-  read.csv <- redskb <- restrict_study_period <- LE_RECT <- target <- tot.landings <- tot.val.landings <- vrd <- y <- NULL
  `%notin%` <- Negate(`%in%`)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  if(exists("x")){
    logbook <- ggleR::load_data(x) # copylogbook <- copy(logbook)
  } else {
    stop("Logbook data missing!")
  }

  ## Temporal dummy variables
  logbook$date <- base::as.Date(strptime(logbook$LE_CDAT, "%d/%m/%Y"))
  logbook$m <- lubridate::month(logbook$date)
  logbook$y <- lubridate::year(logbook$date)

  #### What's the period (in years) of the dataset?
  if(is.null(study_period)){
    study_period <- c(min(logbook$y, na.rm = T):max(logbook$y, na.rm = T))
  }else(study_period)
  logbook <- logbook[y %in% study_period]

  ## Main target species (landed) per fishing day
  ##### Work in progress: Need to map the list of species using the species code
  ##### to LE_KG_species and LE_EURO_species and then figure out the max per row
  ##### This could look something like:
  logbook[, max_kg := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = patterns("^LE_KG_")]
  logbook[, max_eur := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = patterns("^LE_EURO_")]
  logbook[, target := names(.SD)[max.col(replace(.SD, is.na(.SD) | .SD == 0, -Inf), ties.method = "first")], .SDcols = patterns("^LE_EURO_")] ## Target == max landings in value
  logbook[, target := fifelse(target == "LE_EURO_AAS" & LE_EURO_AAS == 0,
                              NA_character_,
                              substr(target, nchar(target) - 2, nchar(target)))]
  ##### If lumpsucker is landed and above 20kg, then we assume that lumpsucker is
  ##### the main target species for that fishing day
  logbook[, target := fifelse(LE_KG_LUM > 20,
                              "LUM",
                              target,
                              na=target)]
  ##### Total catches (landed) in weight (kg)
  logbook[, LE_KG := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("^LE_KG_")] # This adds LE_KG, containing the row-wise sums of all columns whose names start with "LE_KG_"
  ### Total catches (landed) in value (Euro)
  logbook[, LE_EURO := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("^LE_EURO_")]
  ### Remove info on individual species catches/landings
  logbook <- logbook[, grep("^LE_EURO_", names(logbook), value = TRUE) := NULL]
  logbook <- logbook[, grep("^LE_KG_", names(logbook), value = TRUE) := NULL]
  ### Keep only net fisheries
  logbook <- subset(logbook, LE_GEAR %in% c('GN','GND','GNS','GTN','GTR',''))
  logbook <- subset(logbook, stringr::str_starts(LE_MET, "GN") | LE_MET == "")
  logbook <- logbook[!(LE_MET == "" & LE_GEAR == "")]
  ### Mesh size
  logbook[, LE_MSZ := fifelse(LE_MSZ == '' | LE_MSZ == ' ' | LE_MSZ == '.',
                              NA_character_,
                              LE_MSZ, na=LE_MSZ)]
  logbook$LE_MSZ <- as.numeric(logbook$LE_MSZ)
  logbook[, LE_RECT := fifelse(
    LE_RECT == 'NONE', NA_character_, LE_RECT)]
  logbook[, LE_DIV := fifelse(
    LE_DIV == '' | stringr::str_starts(LE_DIV, "NA"), NA_character_, LE_DIV)]

  ## Assign correct name to ICES area /// This is probably only good for DK. Need to test other countries
  logbook[, ices.area := ifelse(LE_DIV == '3AI', 'isefjord',
                                ifelse(LE_DIV == '3AN', '3.a.20',
                                       ifelse(LE_DIV == '3AS', '3.a.21',
                                              ifelse(LE_DIV == '3B', '3.b.23',
                                                     ifelse(LE_DIV == '3C', '3.c.22',
                                                            ifelse(LE_DIV == '3C22', '3.c.22',
                                                                   ifelse(LE_DIV == '3D', '3.d.24', ## Needs to be checked!
                                                                          ifelse(LE_DIV == '3D24', '3.d.24',
                                                                                 ifelse(LE_DIV == '3D25', '3.d.25',
                                                                                        ifelse(LE_DIV == '3D26', '3.d.26',
                                                                                               ifelse(LE_DIV == '4A', '4.a',
                                                                                                      ifelse(LE_DIV == '4B', '4.b',
                                                                                                             ifelse(LE_DIV == '4BX', '3.c.22', ## Yes, this is correct!
                                                                                                                    ifelse(LE_DIV == '4C', '4.c',
                                                                                                                           ifelse(LE_DIV == '3AI3', 'isefjord',
                                                                                                                                  ifelse(LE_DIV == '4L', 'limfjord',
                                                                                                                                         ifelse(LE_DIV == '4R', 'ringk.fjord',
                                                                                                                                                ifelse(LE_DIV == '4N', 'nissum.fjord',
                                                                                                                                                       'NA'))))))))))))))))))]
  ## Vessel length
  logbook <- logbook %>%
    dplyr::mutate(f.length =
                    dplyr::case_when(VE_LEN < 8 ~ "<8m",
                                     dplyr::between(VE_LEN, 8, 10) ~ "8-10m",
                                     dplyr::between(VE_LEN, 10, 12) ~ "10-12m",
                                     dplyr::between(VE_LEN, 12, 15) ~ "12-15m",
                                     VE_LEN > 15 ~ ">15m",
                                     .default = NA_character_))

  ## Eyeballing the mesh size + registered gear + target species,
  ## there are issues. Let's fix the obvious
  logbook$LE_MSZ <- as.numeric(as.character(logbook$LE_MSZ))
  logbook[, LE_MSZ := ifelse(LE_MSZ>=400, NA, LE_MSZ)]

  ## Some rows have info on metier, but not on mesh. We can assume that they use
  ## they use the minimal mesh size in the category. Might need to be completed
  ## with more metier-mesh equivalent from other fleets
  # table(logbook[is.na(LE_MSZ)]$LE_MET)
  ### 1. Are there wrong values (mesh outside the range defined in the metier)?
  logbook[, LE_MSZ := ifelse(LE_MET == "GNS_SPF_>=220_0_0"&
                               LE_MSZ < 220 |
                               LE_MET == "GNS_DEF_>=220_0_0"&
                               LE_MSZ < 220,
                             230,
                             ifelse(LE_MET=="GND_ANA_>=157_0_0"&
                                      LE_MSZ < 157 |
                                      LE_MET=="GNS_ANA_>=157_0_0"&
                                      LE_MSZ < 157 |
                                      LE_MET=="GNS_SPF_>=157_0_0"&
                                      LE_MSZ < 157 |
                                      LE_MET=="GNS_DEF_>=157_0_0"&
                                      LE_MSZ < 157,
                                    157,
                                    ifelse(LE_MET=="GNS_SPF_120-219_0_0"&
                                             LE_MSZ < 120|
                                             LE_MET=="GND_DEF_120-219_0_0"&
                                             LE_MSZ < 120|
                                             LE_MET=="GNS_DEF_120-219_0_0" &
                                             LE_MSZ < 120,
                                           120,
                                           ifelse(LE_MET=="GNS_ANA_110-156_0_0"&
                                                    LE_MSZ < 110|
                                                    LE_MET=="GNS_DEF_110-156_0_0"&
                                                    LE_MSZ < 110|
                                                    LE_MET=="GNS_SPF_110-156_0_0"&
                                                    LE_MSZ < 110,
                                                  110,
                                                  ifelse(LE_MET=="GNS_DEF_100-119_0_0"&
                                                           LE_MSZ < 100|
                                                           LE_MET=="GNS_SPF_100-119_0_0"&
                                                           LE_MSZ < 100,
                                                         100,
                                                         ifelse(LE_MET=="GNS_DEF_90-109_0_0"&
                                                                  LE_MSZ<90|
                                                                  LE_MET=="GNS_ANA_90-109_0_0"&
                                                                  LE_MSZ<90|
                                                                  LE_MET=="GNS_SPF_90-99_0_0" &
                                                                  LE_MSZ<90|
                                                                  LE_MET=="GNS_DEF_90-99_0_0" &
                                                                  LE_MSZ<90,
                                                                90,
                                                                ifelse(LE_MET=="GNS_FWS_>0_0_0"&
                                                                         LE_MSZ<18|
                                                                         LE_MET=='GNS_CRU_>0_0_0'&
                                                                         LE_MSZ<18,
                                                                       18,
                                                                       ifelse(LE_MET=="GNS_SPF_32-109_0_0"&
                                                                                LE_MSZ<32,
                                                                              32,
                                                                              ifelse(LE_MET=="GNS_SPF_10-30_0_0"&
                                                                                       LE_MSZ<10,
                                                                                     10,
                                                                                     ifelse(LE_MET=='GND_SPF_50-70_0_0'&
                                                                                              LE_MSZ<50|
                                                                                              LE_MET=='GNS_SPF_50-70_0_0'&
                                                                                              LE_MSZ<50|
                                                                                              LE_MET=='GNS_DEF_50-70_0_0'&
                                                                                              LE_MSZ<50,
                                                                                            50,
                                                                                            LE_MSZ
                                                                                     ))))))))))]
  ### 2. Are there missing values (undefined mesh, but defined metier)?
  logbook[, LE_MSZ := ifelse(test = !is.na(LE_MSZ), yes = LE_MSZ,
                             ifelse(LE_MET == "GNS_SPF_>=220_0_0" |
                                      LE_MET == "GNS_DEF_>=220_0_0" |
                                      LE_MET == "GNS_CRU_>=220_0_0",
                                    230, # Mean value for these metiers
                                    ifelse(LE_MET == "GND_ANA_>=157_0_0" |
                                             LE_MET == "GNS_ANA_>=157_0_0" |
                                             LE_MET == "GNS_SPF_>=157_0_0" |
                                             LE_MET == "GNS_DEF_>=157_0_0" |
                                             LE_MET ==  "GNS_SPF_120-219_0_0" |
                                             LE_MET ==  "GND_DEF_120-219_0_0" |
                                             LE_MET ==  "GNS_DEF_120-219_0_0" |
                                             LE_MET ==  "GNS_CRU_120-219_0_0",
                                           170, # Mean value for these metiers
                                           ifelse(LE_MET == "GNS_ANA_110-156_0_0" |
                                                    LE_MET == "GNS_DEF_110-156_0_0" |
                                                    LE_MET == "GNS_SPF_110-156_0_0",
                                                  130, # Mean value for these metiers
                                                  ifelse(LE_MET == "GNS_DEF_100-119_0_0" |
                                                           LE_MET == "GNS_SPF_100-119_0_0" |
                                                           LE_MET == "GNS_CRU_100-119_0_0",
                                                         110, # Mean value for these metiers
                                                         ifelse(LE_MET == "GNS_DEF_90-109_0_0" |
                                                                  LE_MET == "GNS_ANA_90-109_0_0" |
                                                                  LE_MET == "GNS_SPF_90-99_0_0" |
                                                                  LE_MET == "GNS_DEF_90-99_0_0" |
                                                                  LE_MET == "GNS_CRU_90-99_0_0" |
                                                                  LE_MET == "GNS_FWS_>0_0_0"|
                                                                  LE_MET == 'GNS_SPF_>0_0_0',
                                                                90, # Mean value for these metiers
                                                                ifelse(LE_MET == "GNS_SPF_32-109_0_0" |
                                                                         LE_MET == "GNS_DEF_32-89_0_0" |
                                                                         LE_MET == "GNS_SPF_32-89_0_0" |
                                                                         LE_MET == "GNS_DEF_50-70_0_0",
                                                                       50, # Mean value for this metiers
                                                                       ifelse(LE_MET == "GNS_SPF_10-30_0_0" |
                                                                                LE_MET == "GNS_ANA_>0_0_0" |
                                                                                LE_MET == "GNS_CAT_>0_0_0" |
                                                                                LE_MET == "GNS_CRU_10-30_0_0" |
                                                                                LE_MET == "GNS_SPF_16-31_0_0",
                                                                              20, # Mean value for this metiers),
                                                                              ifelse(LE_MET == 'GND_SPF_50-70_0_0' |
                                                                                       LE_MET == 'GNS_CRU_50-70_0_0' |
                                                                                       LE_MET == 'GNS_SPF_50-70_0_0' |
                                                                                       LE_MET == 'GNS_DEF_50-70_0_0' |
                                                                                       LE_MET == 'GNS_SPF_>0_0_0',
                                                                                     60,
                                                                                     ifelse(LE_MET == 'GNS_CRU_>0_0_0',
                                                                                            160,
                                                                                            ifelse(LE_MET == 'GNS_CRU_31-49_0_0' |
                                                                                                     LE_MET == 'GNS_DEF_31-49_0_0' |
                                                                                                     LE_MET == 'GNS_SPF_31-49_0_0',
                                                                                                   40,
                                                                                                   ifelse(LE_MET == 'GNS_CRU_71-89_0_0' |
                                                                                                            LE_MET == 'GNS_DEF_71-89_0_0' |
                                                                                                            LE_MET == 'GNS_DEF_71-89_0_0',
                                                                                                          80,
                                                                                                          as.numeric(NA))
                                                                                            )))))))))))]
  ## Add mesh as a factor
  logbook[, f.mesh := data.table::fifelse(LE_MSZ<120, '<120mm',
                                          data.table::fifelse(LE_MSZ>200, '>200mm',
                                                              '120-200mm'))]
  ## Add mean depth and mean distance to shore of the ICES rectnagle the fishing
  ## operations are marked in
  ices.rectangles <- sf::st_read('H:/c-users/Maps/Master layers/ices.rectangles_meandepth_meand2shore.gpkg')
  ices.rectangles$LE_RECT <- ices.rectangles$ICESNAME
  logbook <- logbook[subset(ices.rectangles,
                            select = c('LE_RECT','d2shore.mean','depth.mean')),
                     on = c('LE_RECT')][!is.na(VE_REF)]
  ## When the positions of the fishing operations are indicated, estimate depth
  ## distance to shore at the middle point of the fishing operation. Otherwise,
  ## register fishing location as the centroid of ICES stat. rect.
  logbook[, real.fishing.operation := fifelse(!is.na(LE_SLAT)&
                                                !is.na(LE_ELAT)&
                                                !is.na(LE_SLON)&
                                                !is.na(LE_ELON),
                                              T,
                                              F)]
  suppressWarnings(logbook$LE_SLAT <- as.numeric(as.character(logbook$LE_SLAT)))
  suppressWarnings(logbook$LE_ELAT <- as.numeric(as.character(logbook$LE_ELAT)))
  suppressWarnings(logbook$LE_SLON <- as.numeric(as.character(logbook$LE_SLON)))
  suppressWarnings(logbook$LE_ELON <- as.numeric(as.character(logbook$LE_ELON)))
  logbook[, lat := fifelse(real.fishing.operation==T,
                           (LE_SLAT+LE_ELAT)/2,
                           mapplots::ices.rect(LE_RECT)[,2])]
  logbook[, lon := fifelse(real.fishing.operation==T,
                           (LE_SLON+LE_ELON)/2,
                           mapplots::ices.rect(LE_RECT)[,1])]
  ## Calculate depth and distance to shore at point if the fishing operation is
  ## recorded, otherwise use the mean value of each ICES rectangle
  logbook[, depth := fifelse(real.fishing.operation==T,
                           NA,
                           depth.mean)]
  logbook[, d2shore := fifelse(real.fishing.operation==T,
                           NA,
                           d2shore.mean)]

  # ## TRY AND TEST THIS (DOES NOT WORK YET)
  # library(httr)
  # library(jsonlite)
  # get_emodnet_data <- function(lat, lon) {
  #   url <- paste0("https://www.emodnet-bathymetry.eu/api/bathymetry?lat=", lat, "&lon=", lon)
  #   response <- httr::GET(url)
  #   data <- jsonlite::fromJSON(content(response, "text"))
  #   return(data)
  # }
  # logbook$row.number <-  1:nrow(logbook)
  # missing.points <- subset(logbook, select = c('row.number','lat','lon','depth','d2shore'))
  # missing.points <- missing.points[is.na(depth)]
  # results <- apply(missing.points, 1, function(row) {
  #   lat <- row["latitude"]
  #   lon <- row["longitude"]
  #   get_emodnet_data(lat, lon)
  # })
  # logbook$depth <- sapply(results, function(res) res$depth)
  # logbook$distance_to_shore <- sapply(results, function(res) res$distance_to_shore)


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
  # ices.rectangles <- readRDS('Q:/scientific-projects/cctv-monitoring/data/GIS/ICES_rect.RDS')
  # # ices.rectangles <- sf::read_sf('H:/c-users/Maps/DepthDK/ICES rect depth.gpkg')
  # ices.rectangles$icesrect <- ices.rectangles$ICESNAME
  # logbook <- logbook[subset(ices.rectangles,
  #                           select = c('icesrect','d2shore','depth')),
  #                    on = c('icesrect')]
  # # logbook <- data.table::merge(logbook,
  # #                  subset(ices.rectangles, select = c('ICESNAME','d2shore','depth')),
  # #                  by.x = 'icesrect', by.y = 'ICESNAME', all.x = TRUE)




  # ## Assume that vessels are fishing closest to their home harbour if they do not
  # ## indicate fishing location (icesrect) in Danish logbooks
  # harbours <- ggleR::load_data(path_to_harbour_list)
  # harbours <- data.table::data.table(harbours, key = 'VE_REF')
  # harbours <- unique(logbook, by = 'VE_REF')[, c('VE_REF','VE_LEN')][harbours,
  #                                                                    on = 'VE_REF']
  # # harbours <- data.table::merge(unique(logbook, by = 'VE_REF')[, c('VE_REF','VE_LEN')],
  # #                   harbours,
  # #                   by = 'VE_REF')
  # harbours <- harbours[, c('VE_REF', 'year', 'FT_LHAR', 'home_harbour',
  #                          'VE_LEN'
  #                          # , 'n_trips', 'hel'
  # )]
  # data.table::setnames(harbours, old = 'home_harbour', new = 'lplads')
  # data.table::setkey(harbours, 'lplads')
  #
  # ## Assign coordinates to harbour locations
  # harbours.locations <- data.table::as.data.table(sf::st_read(path_to_harbour_shp))
  # harbours.locations <- harbours.locations[lplads %in% c(harbours$lplads)]
  # data.table::setkey(harbours.locations, 'lplads')
  # harbours[, lplads := data.table::fifelse(lplads == '', Mode(lplads), lplads),
  #          by = c('VE_REF')]
  # harbours[, lplads := data.table::fifelse(lplads == '', FT_LHAR, lplads)]
  # harbours <- data.table::copy(harbours)[harbours.locations,
  #                                        on = 'lplads',
  #                                        `:=`(lon = lgrad,
  #                                             lat = bgrad)]
  # ## Merge with logbook data; VE_REF.year because the same vessel (name) can change
  # ## owner (and thus also home harbour) from one year to the next
  # harbours$VE_REF.year <- paste(harbours$VE_REF, harbours$year, sep='.')
  # harbours <- unique(harbours, by = "VE_REF.year")
  # logbook$VE_REF.year <- paste(logbook$VE_REF, as.character(logbook$y), sep='.')
  #
  # # Merge logbook with harbours to add home_harbour, lon_home, and lat_home
  # logbook <- logbook[subset(harbours,
  #                           select = c('VE_REF.year','lon','lat','lplads')),
  #                    on = 'VE_REF.year']
  # # logbook <- data.table::merge(logbook,
  # #                  subset(harbours,
  # #                         select = c('VE_REF.year','lon','lat','lplads')),
  # #                  by = c('VE_REF.year') )
  # data.table::setnames(logbook, old = c('lon','lat','lplads'),
  #                      new = c('lon_home','lat_home','home_harbour'))
  #
  # ### Assign a fishing location ('icesrect') if there are none
  # ### 1. Most frequent ICES rectangle from the same period (here: same month)?
  # logbook[, newID := paste(VE_REF, m, sep = '_')]
  # logbook[, LE_RECT2 := data.table::fifelse(LE_RECT %notin% '99A9', LE_RECT, NA_character_)]
  # logbook[, mostICESrect := Mode(LE_RECT2), by = c('newID')]
  # logbook[, icesrect := data.table::fifelse(LE_RECT == '99A9' & !is.na(mostICESrect),
  #                                           yes = mostICESrect,
  #                                           no = LE_RECT)]
  # ### 2. If there is no info on location of the effort, then use
  # ###    the harbour location as a proxy
  # logbook[, icesrect := data.table::fifelse(LE_RECT == '' | is.na(LE_RECT) | icesrect == '99A9' | is.na(icesrect),
  #                                           yes = mapplots::ices.rect2(lon_home, lat_home),
  #                                           no = icesrect)]
  #
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
  # ices.rectangles <- readRDS('Q:/scientific-projects/cctv-monitoring/data/GIS/ICES_rect.RDS')
  # # ices.rectangles <- sf::read_sf('H:/c-users/Maps/DepthDK/ICES rect depth.gpkg')
  # ices.rectangles$icesrect <- ices.rectangles$ICESNAME
  # logbook <- logbook[subset(ices.rectangles,
  #                           select = c('icesrect','d2shore','depth')),
  #                    on = c('icesrect')]
  # # logbook <- data.table::merge(logbook,
  # #                  subset(ices.rectangles, select = c('ICESNAME','d2shore','depth')),
  # #                  by.x = 'icesrect', by.y = 'ICESNAME', all.x = TRUE)
  #
  # ## Create an ID for each (unique) fishing day (FD)
  # logbook <- logbook %>%
  #   dplyr::mutate(Date = fngdato) %>%
  #   tidyr::separate(Date, c("y","m","d")) %>%
  #   tidyr::unite(col = Date, c(d,m,y), sep = "-")
  # data.table::setDT(logbook)
  # logbook[, IDFD := paste(VE_REF, Date,sep='.')]
  # logbook[, FD := sum(dplyr::n_distinct(fngdato)),
  #         by = 'FT_REF']
  #
  # logbook$m <- lubridate::month(logbook$fngdato)
  # logbook$y <- lubridate::year(logbook$fngdato)
  # logbook[, quarter := data.table::fifelse(m %in% c(1,2,3), 'Q1',
  #                                          data.table::fifelse(m %in% c(4,5,6), 'Q2',
  #                                                              data.table::fifelse(m %in% c(7,8,9), 'Q3',
  #                                                                                  'Q4')))]
  # logbook$quarter <- factor(logbook$quarter, levels= c('Q1','Q2','Q3','Q4'))
  #
  # ## Main target species (landed) per fishing day
  # ## We have thousands of rows with no information on catch weight. We will set
  # ## all this to be 0kg (to not discard these rows in the process)
  # logbook$vrd <- tidyr::replace_na(logbook$vrd, 0)
  #
  # ## Total value of the catch per fishing day
  # ## We have thousands of rows with no information on catch value We will set
  # ## all this to be 0 (to not discard these rows in the process)
  # logbook$vrd <- tidyr::replace_na(logbook$vrd, 0)
  # logbook <- logbook[
  #   , tot.val.landings := sum(vrd),
  #   by = 'IDFD']
  # ## Total weight of the catch per fishing day
  # ## We have thousands of rows with no information on catch weight. We will set
  # ## all this to be 0kg (to not discard these rows in the process)
  # logbook$hel <- tidyr::replace_na(logbook$hel, 0)
  # logbook <- logbook[
  #   , tot.landings := sum(hel),
  #   by = 'IDFD']
  #
  # ### Main target in VALUE landed ##
  # ## The following will create 2 new variables (latin and target), which are the
  # ## most important catch in terms of landings value per trip
  # logbook <- logbook[logbook[logbook[, .I[base::which.max(vrd)],
  #                                    by = 'IDFD']$V1][, .SD, .SDcols = c('IDFD',
  #                                                                        'latin')],
  #                    on = c('IDFD')]
  # # logbook <- merge(logbook,
  # #                  logbook[logbook[, .I[base::which.max(vrd)],
  # #                                  by = 'IDFD']$V1][, .SD, .SDcols = c('IDFD',
  # #                                                                      'latin')],
  # #                  by = 'IDFD')
  #
  # ### Main target in WEIGHT landed ##
  # ## The following will create 2 new variables (latin and target), which are the
  # ## most important catch in terms of landings weight per trip
  # # logbook <- data.table::merge(logbook,
  # #                  logbook[, .SD[which.max(hel)],
  # #                          by = 'IDFD'][, .SD, .SDcols = c('IDFD', 'latin')],
  # #                  by = 'IDFD')
  # names(logbook)[names(logbook)=="latin.x"] <- "latin"
  # names(logbook)[names(logbook)=="latin.y"] <- "target"
  #
  # ## If lumpsucker is landed, then we assume that lumpsucker is the main target
  # ## species for that fishing day
  # logbook[, target := lapply(.SD, function(x) if(base::any(target == 'Cyclopterus lumpus'))
  #   'Cyclopterus lumpus' else target), by = .(IDFD)]

  return(logbook)
}

