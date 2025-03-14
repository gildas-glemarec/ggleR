#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' https://github.com/CefasRepRes/ICES-VMS-and-Logbook-Data-Call_Cefas/blob/dev_june_2024/EFLALO%20%26%20TACSAT%20Formats.md
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @param study_period A vector of years - e.g., c(2010:2020) - default is NULL
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @export
EFLALO_import <- function(x, #x <- 'Q:/20-forskning/20-dfad/users/ggle/data/EFLALO'
                          study_period = NULL
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
  logbook[, target := data.table::fifelse(target == "LE_EURO_AAS" & LE_EURO_AAS == 0,
                                          NA_character_,
                                          substr(target, nchar(target) - 2, nchar(target)))]
  ##### If lumpsucker is landed and above 20kg, then we assume that lumpsucker is
  ##### the main target species for that fishing day
  logbook[, target := data.table::fifelse(LE_KG_LUM > 20,
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
  logbook[, LE_MSZ := data.table::fifelse(LE_MSZ == '' | LE_MSZ == ' ' | LE_MSZ == '.',
                                          NA_character_,
                                          LE_MSZ, na=LE_MSZ)]
  logbook$LE_MSZ <- as.numeric(logbook$LE_MSZ)
  logbook[, LE_RECT := data.table::fifelse(
    LE_RECT == 'NONE', NA_character_, LE_RECT)]
  logbook[, LE_DIV := data.table::fifelse(
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
  ## Add mean depth and mean distance to shore of the ICES rectangle the fishing
  ## operations are marked in
  ices.rectangles <- sf::st_read('H:/c-users/Maps/Master layers/ices.rectangles_meandepth_meand2shore.gpkg')
  ices.rectangles$LE_RECT <- ices.rectangles$ICESNAME
  logbook <- logbook[subset(ices.rectangles,
                            select = c('LE_RECT','d2shore.mean','depth.mean')),
                     on = c('LE_RECT')][!is.na(VE_REF)]
  ## When the start AND end positions of the logbook event are indicated,
  ## estimate depth and distance to shore at the middle point of the fishing
  ## operation. Otherwise, register the fishing location as the start OR end of
  ## the logbook event
  suppressWarnings(logbook$LE_SLAT <- as.numeric(as.character(logbook$LE_SLAT)))
  suppressWarnings(logbook$LE_ELAT <- as.numeric(as.character(logbook$LE_ELAT)))
  suppressWarnings(logbook$LE_SLON <- as.numeric(as.character(logbook$LE_SLON)))
  suppressWarnings(logbook$LE_ELON <- as.numeric(as.character(logbook$LE_ELON)))
  logbook[, lat := dplyr::case_when(
    ## Start and End of LE known
    !is.na(LE_SLAT)&
      !is.na(LE_ELAT)&
      !is.na(LE_SLON)&
      !is.na(LE_ELON) ~
    (LE_SLAT+LE_ELAT)/2,
    ## Only Start of LE known
    !is.na(LE_SLAT)&
      is.na(LE_ELAT)&
      !is.na(LE_SLON)&
      is.na(LE_ELON) ~ LE_SLAT,
    ## Only End of LE known
    is.na(LE_SLAT)&
      !is.na(LE_ELAT)&
      is.na(LE_SLON)&
      !is.na(LE_ELON) ~ LE_ELAT,
    .default = NA)]
  logbook[, lon := data.table::fifelse(!is.na(LE_SLAT)&
                                         !is.na(LE_ELAT)&
                                         !is.na(LE_SLON)&
                                         !is.na(LE_ELON),
                                       (LE_SLAT+LE_ELAT)/2,
                                       mapplots::ices.rect(LE_RECT)[,1])]


  # logbook[, real.fishing.operation := data.table::fifelse(!is.na(LE_SLAT)&
  #                                                           !is.na(LE_ELAT)&
  #                                                           !is.na(LE_SLON)&
  #                                                           !is.na(LE_ELON),
  #                                                         T,
  #                                                         F)]
  # logbook[, lat := data.table::fifelse(real.fishing.operation==T,
  #                                      (LE_SLAT+LE_ELAT)/2,
  #                                      mapplots::ices.rect(LE_RECT)[,2])]
  # logbook[, lon := data.table::fifelse(real.fishing.operation==T,
  #                                      (LE_SLON+LE_ELON)/2,
  #                                      mapplots::ices.rect(LE_RECT)[,1])]
  # # Calculate depth and distance to shore at point if the fishing operation is
  # # recorded, otherwise use the mean value of each ICES rectangle
  # logbook[, depth := data.table::fifelse(real.fishing.operation==T,
  #                                        depth.mean,
  #                                        NA)]
  # logbook[, d2shore := data.table::fifelse(real.fishing.operation==T,
  #                                          d2shore.mean,
  #                                          NA)]






  # # ## TRY AND TEST THIS (DOES NOT WORK YET)
  # library(httr)
  # library(gdalUtilities)
  # # library(jsonlite)
  # # get_emodnet_data <- function(lat, lon) {
  # #   url <- paste0("https://www.emodnet-bathymetry.eu/api/bathymetry?lat=", lat, "&lon=", lon)
  # #   response <- httr::GET(url)
  # #   data <- jsonlite::fromJSON(content(response, "text"))
  # #   return(data)
  # # }
  # # logbook$row.number <-  1:nrow(logbook)
  # # missing.points <- subset(logbook, select = c('row.number','lat','lon','depth','d2shore'))
  # # missing.points <- missing.points[is.na(depth)]
  # # results <- apply(missing.points, 1, function(row) {
  # #   lat <- row["latitude"]
  # #   lon <- row["longitude"]
  # #   get_emodnet_data(lat, lon)
  # # })
  # # logbook$depth <- sapply(results, function(res) res$depth)
  # # logbook$distance_to_shore <- sapply(results, function(res) res$distance_to_shore)
  #
  # logbook$row.number <-  1:nrow(logbook)
  # missing.points <- subset(logbook, select = c('row.number','lat','lon',
  #                                              'depth','d2shore'))
  # missing.points <- missing.points[is.na(depth)]
  #
  #
  #
  # # install.packages("remotes")
  # ### WFS
  # # remotes::install_github("EMODnet/EMODnetWFS")
  # library(ows4R)
  # library(emodnet.wfs)
  # emodnet_bathy_client <- ows4R::WFSClient$new(
  #   "https://ows.emodnet-bathymetry.eu/wfs",
  #   serviceVersion = "2.0.0")
  # # View(emodnet_get_wfs_info(emodnet_bathy_client))
  # # depth.highres.layers <- emodnet_get_layers(
  # #   wfs = emodnet_bathy_client,
  # #   layers = "hr_bathymetry_area",
  # #   simplify = TRUE,
  # #   outputFormat = "application/json"
  # # )
  # # mapview::mapview(depth.highres.layers, burst = TRUE)
  # # depth.tiles.layers <- emodnet_get_layers(
  # #   wfs = emodnet_bathy_client,
  # #   layers = "download_tiles",
  # #   simplify = TRUE,
  # #   outputFormat = "application/json"
  # # )
  # # mapview::mapview(depth.tiles.layers, burst = TRUE)
  # bounding.box <- paste(min(missing.points$lon),min(missing.points$lat),
  #                       max(missing.points$lon),max(missing.points$lat),
  #                       "EPSG:4326", sep = ',')
  # depth.contours.layers.simple <- emodnet.wfs::emodnet_get_layers(
  #   wfs = emodnet_bathy_client,
  #   layers = "contours",
  #   simplify = TRUE,
  #   # outputFormat = "CSV",
  #   bbox = bounding.box)
  # res <- rerddapXtracto::rxtracto(bathyInfo, parameter = parameter,
  #                                 xcoord = xcoord , ycoord = ycoord)
  #
  # # query <- list(service = "WFS",
  # #               request = "GetFeature",
  # #               typeName = "emodnet:contours",
  # #               outputFormat = "json",
  # #               propertyname = "elevation",
  # #               CRS = "EPSG:4326",
  # #               CQL_FILTER = sprintf("INTERSECTS(geom,POINT(%s %s))",
  # #                                    missing.points$lon, missing.points$lat))
  # # result <- GET(emodnet_bathy_client, query = query)
  # # result
  #
  #
  #
  #
  #
  #
  #
  #
  # depth.contours.layers.simple %>%
  #   # sf::st_cast(to = "MULTILINESTRING") %>%
  #   mapview::mapview(burst = TRUE, legend = FALSE)
  #
  #
  #
  # %>%
  #   sf::st_as_sf()
  # # mapview::mapview(depth.contours.layers.simple, burst = TRUE)
  #
  #
  #
  # # ### WCS
  # # # remotes::install_github("EMODnet/EMODnetWCS")
  # # # library(EMODnetWCS)
  # # wcs <- EMODnetWCS::emdn_init_wcs_client(service = "bathymetry")
  # # # EMODnetWCS::emdn_get_wcs_info(service = "bathymetry")$coverage_details
  # # # EMODnetWCS::emdn_get_coverage_ids(wcs)
  # # cov <- EMODnetWCS::emdn_get_coverage(wcs,
  # #                                      coverage_id = "emodnet__mean_2020",
  # #                                      bbox = c(xmin = min(missing.points$lon),
  # #                                               ymin = min(missing.points$lat),
  # #                                               xmax = max(missing.points$lon),
  # #                                               ymax = max(missing.points$lat)),
  # #                                      nil_values_as_na = TRUE
  # # )
  #
  #
  #
  #
  #
  #
  #
  #
  # # ######################################
  # # remotes::install_github("ropensci/rerddap")
  # # install.packages(rerddapXtracto, dependencies = TRUE)
  # require("rerddap")
  # require("rerddapXtracto")
  # urlBase <- "https://erddap.emodnet.eu/erddap/griddap/" #"https://erddap.emodnet.eu/erddap/"
  # parameter <- "altitude"
  # bathyInfo <- rerddap::info(datasetid = "dtm_2020_v2_e0bf_e7e4_5b8f",# "emodnet_bathy_2020_full",
  #                            url = urlBase)
  # xcoord <- missing.points$lon[1]
  # ycoord <- missing.points$lat[1]
  #
  # # res <-  rerddap::tabledap(bathyInfo, fields = parameter)
  #
  #
  # res <- rerddapXtracto::rxtracto(bathyInfo, parameter = parameter,
  #                                 xcoord = xcoord , ycoord = ycoord)
  # #
  # # for (i in 1:10){
  # #   missing.points[i]
  # # }
  # #
  # #
  # #
  # #
  # #
  # # tiles <- emodnet_get_layers(service = "bathymetry", layers = "download_tiles")
  # # tile_D5 <- tiles %>%
  # #   filter(dtm_release = 2022 & )
  # #
  # #
  # # layers <- c("download_tiles", "hr_bathymetry_area")
  # # bathy_emodnet_sub <- emodnet_get_layers(service = "bathymetry",
  # #                                         layers = layers)
  # #https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/cf51df64-56f9-4a99-b1aa-36b8d7b743a1
  # # #############################
  #
  # # # # https://tutorials.inbo.be/tutorials/spatial_wfs_services/
  #
  # wfs_emodnet_bathy <- "https://ows.emodnet-bathymetry.eu/wfs"
  # emodnet_bathy_client <- ows4R::WFSClient$new(wfs_emodnet_bathy,
  #                                              serviceVersion = "2.0.0")
  # # # emodnet_bathy_client$getFeatureTypes(pretty = TRUE)
  # # # emodnet_bathy_client$getFeatureTypes() %>%
  # # #   purrr::map_chr(function(x){x$getName()})
  # # # emodnet_bathy_client$getFeatureTypes() %>%
  # # #   purrr::map_chr(function(x){x$getTitle()})
  # # ## available operations of the WFS
  # # emodnet_bathy_client$
  # #   getCapabilities()$
  # #   getOperationsMetadata()$
  # #   getOperations() %>%
  # #   purrr::map_chr(function(x){x$getName()})
  # # ## available output formats
  # # emodnet_bathy_client$
  # #   getCapabilities()$
  # #   getOperationsMetadata()$
  # #   getOperations() %>%
  # #   purrr::map(function(x){x$getParameters()}) %>%
  # #   purrr::pluck(3, "outputFormat")
  # # ## bounding boxes for all layers
  # # emodnet_bathy_client$
  # #   getCapabilities()$
  # #   getFeatureTypes() %>%
  # #   purrr::map(function(x){x$getBoundingBox()})
  # # ## Abstract of the layer?
  # # emodnet_bathy_client$
  # #   getCapabilities()$
  # #   getFeatureTypes() %>%
  # #   purrr::map_chr(function(x){x$getAbstract()})
  # # ## Feature types?
  # # emodnet_bathy_client$getFeatureTypes(pretty = TRUE)
  # ## Write our query
  # properties_of_interest <- "elevation"
  # emodnet_bathy_query <- list(service = "WFS",
  #                             request = "GetFeature",
  #                             typeName = "emodnet:countours",
  #                             outputFormat = "csv",
  #                             # srsName = "EPSG:4326"
  #                             # # ,
  #                             propertyname = as.character(paste(properties_of_interest,
  #                                                               collapse = ",")),
  #                             CRS = "EPSG:4326"
  #                             # , CQL_FILTER = sprintf("INTERSECTS(geom,POINT(%s %s))",
  #                             # missing.points$lon, missing.points$lat
  # )
  #
  # extract_bathy_data <- function(lon,
  #                                lat,
  #                                properties_of_interest) {
  #   if (missing(properties_of_interest)) {
  #     properties_of_interest <- "elevation"
  #     message("Defaulting to elevation. To avoid this message provide properties of interest in the function call.")
  #   }
  #   # dealing with point data inside a certain polygon of the soil map:
  #   wfs_emodnet_bathy <- "https://ows.emodnet-bathymetry.eu/wfs"
  #   query = list(service = "WFS",
  #                request = "GetFeature",
  #                # version = "1.1.0",
  #                typeName = "emodnet:countours",
  #                outputFormat = "json",
  #                propertyname = as.character(paste(properties_of_interest,
  #                                                  collapse = ",")),
  #                CRS = "EPSG:4623",
  #                CQL_FILTER = paste("INTERSECTS(geom","POINT(",
  #                                   missing.points$lon[1],
  #                                   ",",
  #                                   missing.points$lat[1],
  #                                   ")")) # INTERSECT OPERATOR
  #
  #   result <- httr::GET(wfs_emodnet_bathy, query = query)
  #   if (grepl("ExceptionText", content(result, "text"))) {
  #     stop(paste(paste(properties_of_interest, collapse = ", "),"is not available for emodnet:countours."))
  #   }
  #   parsed <- jsonlite::fromJSON(content(result, "text"))
  #   bathy_info_df <- parsed$features$properties
  #   # if else to catch cases where a point falls outside the map
  #   if (is.null(bathy_info_df)) {
  #     as.data.frame(
  #       matrix(rep(NA, length(properties_of_interest)),
  #              nrow = 1,
  #              dimnames = list(NULL, properties_of_interest)))
  #   } else {
  #     bathy_info_df
  #   }
  # }
  #
  # missing.points %>%
  #   group_by(row.number) %>%
  #   summarise(extract_bathy_data(lon,
  #                                lat,
  #                                properties_of_interest = properties_of_interest)) %>%
  #   knitr::kable()
  #
  #
  #
  # # df <- read.csv(textConnection(httr::content(result, 'text')))
  # # knitr::kable(df)
  #
  # extract_depth(
  #   lat = lat,
  #   lon = lon,
  #   properties_of_interest = properties_of_interest) %>%
  #   knitr::kable()
  #
  # missing.points %>%
  #   group_by(row.number) %>%
  #   summarise(extract_depth(lat,lon,
  #                           properties_of_interest = properties_of_interest)) %>%
  #   knitr::kable()
  #
  # #############################


  missing.points <- ggleR::get.depth(missing.points)
  ## Add info on distance (m) to nearest point on shore
  coastline <- sf::st_read(path.to.coastline)
  x_sf <- x  %>%
    sf::st_as_sf(coords = c('lon.haul','lat.haul'), na.fail = FALSE,
                 crs = 4326) %>%
    sf::st_transform(3035)
  distances <- sapply(1:nrow(x_sf), function(i) {
    point <- x_sf[i, ]
    min(sf::st_distance(point, coastline))
  })
  x$d2shore <- distances
  ## In the map we use here, there are a couple a islets in the Sound that
  ## do not appear to be correct. As a result, we could rarely have d2shore=0
  ## Fix by forcing a minimum d2shore of 20 metres
  x <- x %>%
    dplyr::mutate(d2shore = ifelse(d2shore == 0, 20, d2shore))

  return(logbook)
}

