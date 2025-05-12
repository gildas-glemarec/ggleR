#' Format logbook / landings data to merge with EM data
#' Dataset preparations
#' Format the input dataset following the EFLALO2 way (google TACSAT and EFLALO Formats and check the Github page)
#' @param x path to the directory where the logbook & sales notes are stored as .csv
#' @param study_period A vector of years - e.g., c(2010:2020) - default is NULL
#' @return A dataset with all notes/annotations in long format, where rows are unique for hauls for no or one bycatch within that haul (each additional bycatch is listed as one supplementary row).
#' @import data.table
#' @export
EFLALO_import <- function(x,
                          study_period = NULL
){

  . <- LE_D2S <- LE_GEAR <- LE_CYEAR <- FT_MAX.KG <- FT_MAX.EUR <- FT_TARGET <- LE_EURO_AAS <- LE_KG_LUM <- LE_KG <- LE_EURO <- VE_F.LEN <- LE_MS <- LE_LAT <- LE_LON <- quarter <- vessel.length <- LE_DIV <- Date <- FD <- IDFD <- d <- eart <- LE_F.MESH <- VE_REF <- fngdato <- hel <- home_harbour <- i.bgrad <- i.lat <- i.lgrad <- i.lon <- i.lplads <- LE_ices.area <- icesrect <- lat <- lat_home <- latin <- lon <- lon_home <- lplads <- m <- LE_MSZ <- mesh <- metier_level6_ret <- LE_MET <- path <-  read.csv <- redskb <- restrict_study_period <- LE_RECT <- target <- tot.landings <- tot.val.landings <- vrd <- y <- NULL

  `%notin%` <- Negate(`%in%`)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  if(exists("x")){
    logbook <- ggleR::load_data(x)
  } else {
    stop("Logbook data missing!")
  }

  ### Keep only net fisheries and missing gears
  logbook <- subset(logbook, LE_GEAR %in% c('GN','GND','GNS','GTN','GTR',''))
  logbook <- subset(logbook, stringr::str_starts(LE_MET, "GN") | LE_MET == "")
  logbook <- logbook[!(LE_MET == "" & LE_GEAR == "")]

  ## Temporal dummy variables
  logbook$LE_CDAT_ymd <- base::as.Date(strptime(logbook$LE_CDAT, "%d/%m/%Y"))
  logbook$LE_CMONTH <- lubridate::month(logbook$LE_CDAT_ymd)
  logbook$LE_CYEAR <- lubridate::year(logbook$LE_CDAT_ymd)

  #### What's the period (in years) of the dataset?
  if(is.null(study_period)){
    study_period <- c(min(logbook$LE_CYEAR, na.rm = T):max(logbook$LE_CYEAR, na.rm = T))
  }else(study_period)
  logbook <- logbook[LE_CYEAR %in% study_period]

  ## Main target species (landed) per fishing day
  logbook[, FT_TARGET.KG := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = patterns("^LE_KG_")]
  logbook[, FT_TARGET.EUR := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = patterns("^LE_EURO_")]
  logbook[, FT_TARGET := names(.SD)[max.col(replace(.SD, is.na(.SD), -Inf),
                                            ties.method = "first")],
          .SDcols = patterns("^LE_EURO_")] ## Target == max landings in value
  logbook[, FT_TARGET := data.table::fifelse(is.na(FT_TARGET),
                                             NA_character_,
                                             substr(FT_TARGET,
                                                    nchar(FT_TARGET) - 2,
                                                    nchar(FT_TARGET)))]
  ##### Replace NA in FT_TARGET.KG with 0 for consistency
  logbook[, FT_TARGET.KG := fifelse(is.na(FT_TARGET.KG), 0, FT_TARGET.KG)]
  ##### If lumpfish is landed and above 20kg, then we assume that lumpfish is
  ##### the main target species for that fishing day
  logbook[, FT_TARGET := data.table::fifelse(LE_KG_LUM > 20,
                                             "LUM",
                                             FT_TARGET,
                                             na = FT_TARGET)]

  ##### Total catches (landed) in weight (kg)
  logbook[# Add LE_KG, containing the row-wise sums of all columns whose names start with "LE_KG_"
    , LE_KG := rowSums(.SD, na.rm = TRUE),
    .SDcols = patterns("^LE_KG_")]
  ### Total catches (landed) in value (Euro)
  logbook[, LE_EURO := rowSums(.SD, na.rm = TRUE),
          .SDcols = patterns("^LE_EURO_")]
  ### Remove info on individual species catches/landings
  logbook <- logbook[, grep("^LE_EURO_", names(logbook), value = TRUE) := NULL]
  logbook <- logbook[, grep("^LE_KG_", names(logbook), value = TRUE) := NULL]
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
  logbook[, LE_ices.area := dplyr::case_when(
    LE_DIV == '3AI' ~ 'isefjord',
    LE_DIV == '3AN'~ '3.a.20',
    LE_DIV == '3AS'~ '3.a.21',
    LE_DIV == '3B'~ '3.b.23',
    LE_DIV == '3C'~ '3.c.22',
    LE_DIV == '3C22'~ '3.c.22',
    LE_DIV == '3D'~ '3.d.24', ## Needs to be checked!
    LE_DIV == '3D24'~ '3.d.24',
    LE_DIV == '3D25'~ '3.d.25',
    LE_DIV == '3D26'~ '3.d.26',
    LE_DIV == '4A'~ '4.a',
    LE_DIV == '4B'~ '4.b',
    LE_DIV == '4BX'~ '3.c.22', ## Yes, this is correct!
    LE_DIV == '4C'~ '4.c',
    LE_DIV == '3AI3'~ 'isefjord',
    LE_DIV == '4L'~ 'limfjord',
    LE_DIV == '4R'~ 'ringk.fjord',
    LE_DIV == '4N'~ 'nissum.fjord',
    .default = NA_character_)]

  ## Vessel length category
  logbook[, VE_F.LEN := dplyr::case_when(VE_LEN < 8 ~ "<8m",
                                         dplyr::between(VE_LEN, 8, 10) ~ "8-10m",
                                         dplyr::between(VE_LEN, 10, 12) ~ "10-12m",
                                         dplyr::between(VE_LEN, 12, 15) ~ "12-15m",
                                         VE_LEN > 15 ~ ">15m",
                                         .default = NA_character_) ]
  ## Eyeballing the mesh size + registered gear + target species,
  ## there are issues. Let's fix the obvious
  logbook$LE_MSZ <- as.numeric(as.character(logbook$LE_MSZ))
  logbook[, LE_MSZ := ifelse(LE_MSZ>=400, NA, LE_MSZ)]

  ## Some rows have info on metier, but not on mesh. We can assume that they use
  ## they use the minimal mesh size in the category. Might need to be completed
  ## with more metier-mesh equivalent from other fleets
  # table(logbook[is.na(LE_MSZ)]$LE_MET)
  ### 1. Are there wrong values (mesh outside the range defined in the metier)?
  logbook[, LE_MSZ := dplyr::case_when(
    LE_MET == "GNS_SPF_>=220_0_0"&
      LE_MSZ < 220 |
      LE_MET == "GNS_DEF_>=220_0_0"&
      LE_MSZ < 220 ~
      230,
    LE_MET=="GND_ANA_>=157_0_0"&
      LE_MSZ < 157 |
      LE_MET=="GNS_ANA_>=157_0_0"&
      LE_MSZ < 157 |
      LE_MET=="GNS_SPF_>=157_0_0"&
      LE_MSZ < 157 |
      LE_MET=="GNS_DEF_>=157_0_0"&
      LE_MSZ < 157 ~
      157,
    LE_MET=="GNS_SPF_120-219_0_0"&
      LE_MSZ < 120|
      LE_MET=="GND_DEF_120-219_0_0"&
      LE_MSZ < 120|
      LE_MET=="GNS_DEF_120-219_0_0" &
      LE_MSZ < 120 ~
      120,
    LE_MET=="GNS_ANA_110-156_0_0"&
      LE_MSZ < 110|
      LE_MET=="GNS_DEF_110-156_0_0"&
      LE_MSZ < 110|
      LE_MET=="GNS_SPF_110-156_0_0"&
      LE_MSZ < 110 ~
      110,
    LE_MET=="GNS_DEF_100-119_0_0"&
      LE_MSZ < 100|
      LE_MET=="GNS_SPF_100-119_0_0"&
      LE_MSZ < 100 ~
      100,
    LE_MET=="GNS_DEF_90-109_0_0"&
      LE_MSZ<90|
      LE_MET=="GNS_ANA_90-109_0_0"&
      LE_MSZ<90|
      LE_MET=="GNS_SPF_90-99_0_0" &
      LE_MSZ<90|
      LE_MET=="GNS_DEF_90-99_0_0" &
      LE_MSZ<90 ~
      90,
    LE_MET=="GNS_FWS_>0_0_0"&
      LE_MSZ<18|
      LE_MET=='GNS_CRU_>0_0_0'&
      LE_MSZ<18 ~
      18,
    LE_MET=="GNS_SPF_32-109_0_0"&
      LE_MSZ<32 ~
      32,
    LE_MET=="GNS_SPF_10-30_0_0"&
      LE_MSZ<10 ~
      10,
    LE_MET=='GND_SPF_50-70_0_0'&
      LE_MSZ<50|
      LE_MET=='GNS_SPF_50-70_0_0'&
      LE_MSZ<50|
      LE_MET=='GNS_DEF_50-70_0_0'&
      LE_MSZ<50 ~
      50,
    .default = LE_MSZ)]

  ### 2. Are there missing values (undefined mesh, but defined metier)?
  logbook[, LE_MSZ := ifelse(test = !is.na(LE_MSZ), yes = LE_MSZ,
                             dplyr::case_when(
                               LE_MET == "GNS_SPF_>=220_0_0" |
                                 LE_MET == "GNS_DEF_>=220_0_0" |
                                 LE_MET == "GNS_CRU_>=220_0_0" ~
                                 230, # Mean value for these metiers
                               LE_MET == "GND_ANA_>=157_0_0" |
                                 LE_MET == "GNS_ANA_>=157_0_0" |
                                 LE_MET == "GNS_SPF_>=157_0_0" |
                                 LE_MET == "GNS_DEF_>=157_0_0" |
                                 LE_MET ==  "GNS_SPF_120-219_0_0" |
                                 LE_MET ==  "GND_DEF_120-219_0_0" |
                                 LE_MET ==  "GNS_DEF_120-219_0_0" |
                                 LE_MET ==  "GNS_CRU_120-219_0_0" ~
                                 170, # Mean value for these metiers
                               LE_MET == "GNS_ANA_110-156_0_0" |
                                 LE_MET == "GNS_DEF_110-156_0_0" |
                                 LE_MET == "GNS_SPF_110-156_0_0" ~
                                 130, # Mean value for these metiers
                               LE_MET == "GNS_DEF_100-119_0_0" |
                                 LE_MET == "GNS_SPF_100-119_0_0" |
                                 LE_MET == "GNS_CRU_100-119_0_0" ~
                                 110, # Mean value for these metiers
                               LE_MET == "GNS_DEF_90-109_0_0" |
                                 LE_MET == "GNS_ANA_90-109_0_0" |
                                 LE_MET == "GNS_SPF_90-99_0_0" |
                                 LE_MET == "GNS_DEF_90-99_0_0" |
                                 LE_MET == "GNS_CRU_90-99_0_0" |
                                 LE_MET == "GNS_FWS_>0_0_0"|
                                 LE_MET == 'GNS_SPF_>0_0_0' ~
                                 90, # Mean value for these metiers
                               LE_MET == "GNS_SPF_32-109_0_0" |
                                 LE_MET == "GNS_DEF_32-89_0_0" |
                                 LE_MET == "GNS_SPF_32-89_0_0" |
                                 LE_MET == "GNS_DEF_50-70_0_0" ~
                                 50, # Mean value for this metiers
                               LE_MET == "GNS_SPF_10-30_0_0" |
                                 LE_MET == "GNS_ANA_>0_0_0" |
                                 LE_MET == "GNS_CAT_>0_0_0" |
                                 LE_MET == "GNS_CRU_10-30_0_0" |
                                 LE_MET == "GNS_SPF_16-31_0_0" ~
                                 20, # Mean value for this metiers)
                               LE_MET == 'GND_SPF_50-70_0_0' |
                                 LE_MET == 'GNS_CRU_50-70_0_0' |
                                 LE_MET == 'GNS_SPF_50-70_0_0' |
                                 LE_MET == 'GNS_DEF_50-70_0_0' |
                                 LE_MET == 'GNS_SPF_>0_0_0' ~
                                 60,
                               LE_MET == 'GNS_CRU_>0_0_0' ~
                                 160,
                               LE_MET == 'GNS_CRU_31-49_0_0' |
                                 LE_MET == 'GNS_DEF_31-49_0_0' |
                                 LE_MET == 'GNS_SPF_31-49_0_0' ~
                                 40,
                               LE_MET == 'GNS_CRU_71-89_0_0' |
                                 LE_MET == 'GNS_DEF_71-89_0_0' |
                                 LE_MET == 'GNS_DEF_71-89_0_0' ~
                                 80,
                               .default = NA_integer_))]

  ## Add mesh as a factor
  logbook[, LE_F.MESH := data.table::fifelse(LE_MSZ<120, '<120mm',
                                             data.table::fifelse(LE_MSZ>200, '>200mm',
                                                                 '120-200mm'))]
  ## Add mean depth and mean distance to shore of the ICES rectangle the fishing
  ## operations are marked in
  ices.rectangles <- sf::st_read("./data/ices.rectangles_meandepth_meand2shore.gpkg",
                                 quiet = TRUE)
  ices.rectangles$LE_RECT <- ices.rectangles$ICESNAME
  logbook <- logbook[subset(ices.rectangles,
                            select = c('LE_RECT','d2shore.mean','depth.mean')),
                     on = c('LE_RECT')][!is.na(VE_REF)]

  ### For clarity, the depth and distance to shore in the ICES rectangles are
  ### respectively FT_DEP and FT_D2S and the depth and distance to shore of the
  ### fishing operations are LE_DEP and LE_D2S. The 2 latest are only known when
  ### the positions of the gears are known.
  data.table::setnames(logbook, c("depth.mean","d2shore.mean"), c("FT_DEP","FT_D2S"))

  ## When the start AND end positions of the logbook event are indicated,
  ## estimate depth and distance to shore at the middle point of the fishing
  ## operation. Otherwise, register the fishing location as the start OR end of
  ## the logbook event
  suppressWarnings(logbook$LE_SLAT <- as.numeric(as.character(logbook$LE_SLAT)))
  suppressWarnings(logbook$LE_ELAT <- as.numeric(as.character(logbook$LE_ELAT)))
  suppressWarnings(logbook$LE_SLON <- as.numeric(as.character(logbook$LE_SLON)))
  suppressWarnings(logbook$LE_ELON <- as.numeric(as.character(logbook$LE_ELON)))
  #### Latitude (fishing operation)
  logbook[, LE_LAT := dplyr::case_when(
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
  #### Longitude (fishing operation)
  logbook[, LE_LON := dplyr::case_when(
    ## Start and End of LE known
    !is.na(LE_SLAT)&
      !is.na(LE_ELAT)&
      !is.na(LE_SLON)&
      !is.na(LE_ELON) ~
      (LE_SLON+LE_ELON)/2,
    ## Only Start of LE known
    !is.na(LE_SLAT)&
      is.na(LE_ELAT)&
      !is.na(LE_SLON)&
      is.na(LE_ELON) ~ LE_SLON,
    ## Only End of LE known
    is.na(LE_SLAT)&
      !is.na(LE_ELAT)&
      is.na(LE_SLON)&
      !is.na(LE_ELON) ~ LE_ELON,
    .default = NA)]

  #### Depth (fishing operation)
  # logbook$LE_DEP <- mapply(ggleR::get_depth_EMODNET,
  #                          logbook$LE_LAT,
  #                          logbook$LE_LON)

  ### Define the ERDDAP dataset ID and the variable you want to retrieve
  #### https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/cf51df64-56f9-4a99-b1aa-36b8d7b743a1
  dataset_id <- "bathymetry_dtm_2024"
  variable <- "elevation"
  erddap_url <- "https://erddap.emodnet.eu/erddap/"
  out <- rerddap::info(datasetid = dataset_id,
                       url = erddap_url)
  depth_EMODNET <- function(LE_LAT, LE_LON) {

    if ( !is.na(LE_LAT) &
         !is.na(LE_LON) &
         LE_LAT > 15.000520833333333 &
         LE_LAT < 89.99947916660017 &
         LE_LON > -35.99947916666667 &
         LE_LON < 42.99947916663591){
      # Query the ERDDAP server
      Sys.sleep(0.1)
      result <- suppressMessages(
        rerddap::griddap(
          out,
          fields = variable,
          latitude = c(LE_LAT,LE_LAT),
          longitude = c(LE_LON,LE_LON))
      )

      # Extract the depth value
      depth <- result$data[[variable]]

    } else(
      depth <- NA_integer_
    )

    return(depth)
  }

  logbook$LE_DEP <- NA
  chunk_size <- 500
  num_chunks <- ceiling(nrow(logbook) / chunk_size)
  depth_in_chuncks <- list()

  for (i in 1:num_chunks) {
    # Calculate the start and end indices for the current chunk
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(logbook))

    # Extract the current chunk of data
    logbook_chunk <- logbook[start_idx:end_idx, ]

    # Query the server with the current chunk
    one_chunck <-  mapply(depth_EMODNET,
                          logbook_chunk$LE_LAT,
                          logbook_chunk$LE_LON)
    # Store the result
    depth_in_chuncks[[i]] <- one_chunck

    # Print progress
    cat("Processed chunk", i, "of", num_chunks, "\n")
    Sys.sleep(1)

  }
  logbook$LE_DEP <- unlist(depth_in_chuncks)

  #### Distance to shore (fishing operation)
  logbook$LE_D2S <- ggleR::get_d2shore(x = logbook)

  ###### Some points appear to be on land or in harbour ( is.NaN(LE_DEP) ). Fix:
  logbook[, LE_DEP := ifelse(is.nan(LE_DEP), NA_integer_, LE_DEP)]
  logbook[, LE_D2S := ifelse(is.nan(LE_DEP), NA_integer_, LE_D2S)]
  logbook[, LE_D2S := ifelse(is.na(LE_DEP)&LE_D2S==0, NA_integer_, LE_D2S)]

  ## In the map we use here, there are a couple a islets in the Sound that
  ## do not appear to be correct. As a result, we could rarely have d2shore = 0
  ## Fix by forcing a minimum d2shore of 20 metres
  logbook[, LE_D2S := ifelse(LE_D2S == 0, 20, LE_D2S)]

  return(logbook)
}
