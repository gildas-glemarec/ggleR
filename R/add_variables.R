#' Add variables to the extracted EM dataset
#' Take in the Black Box data from the BBimport function and adds a number of important parameters
#' @param x Data output from function BBimport
#' @param give_me_more Should optional variables not relevant for RDBES be created too (default is TRUE)?
#' @param study_period Should all years (default), or only specific years be compiled (defined as c(year1, year2, etc.))?
#' @param path_to_soak Some soak time info is missing (not found in BB). Manual fix based on a prior extraction (input is a csv file)
#' @param path.to.coastline ESRI Shapefile of the region (used to estimate distance to shore). By default, the file is Europe (Bounding box:  xmin: 943609.8 ymin: -375446 xmax: 7601958 ymax: 6825119; Projected CRS: ETRS89-extended / LAEA Europe)
#' @param path.to.old.notes CSV file wit h a previous version of Notes and Annotations
#' @return Same dataset with additional columns
#' @export
add_variables <- function(x = data_work,
                          study_period = NULL,
                          give_me_more = TRUE,
                          path_to_soak = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/blackbox extractions/soak/",
                          path.to.coastline = "Q:/20-forskning/12-gis/Dynamisk/GEOdata2020/BasicLayers/Coastlines/Europe/EEA Europe/EEA_Coastline_20170228.shp",
                          path.to.raster = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/GIS/D5_2020.tif",
                          path.to.old.notes = "Q:/10-forskningsprojekter/faste-cctv-monitoring/data/processed data/NotesAnnotations.csv") {

  . <- depth <- icesrect <- lon.haul <- lat.haul <- lon <- lat <- time.bc <- IDhaul <- soak <- i.soak <- d2shore <- data_work <- y <- m <- d <- quarter <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL

  x$rnum <- as.integer(row.names(x))
  data.table::setDT(x)
  data.table::setkeyv(x, "rnum")
  x$vessel <- as.factor(x$vessel)
  x$mesh.colour <- as.factor(x$mesh.colour)
  x$review.info <- as.factor(x$review.info)
  x$IDFD <- as.factor(x$IDFD)
  x$IDhaul <- as.factor(x$IDhaul)
  x$IDbc <- as.factor(x$IDbc)
  x$time.start <- lubridate::ymd_hms(x$time.start)
  x$time.stop <- lubridate::ymd_hms(x$time.stop)
  x$time.bc <- lubridate::ymd_hms(x$time.bc)
  x$colour.name <- as.factor(x$colour.name)
  x$note.type <- as.factor(x$note.type)
  x$mitigation <- as.factor(x$mitigation)
  x$mitigation_type <- as.factor(x$mitigation_type)
  x$date <- lubridate::dmy(x$date)
  x[, y := lubridate::year(date)]
  if(is.null(study_period)){
    study_period <- c(min(x$y, na.rm = T):max(x$y, na.rm = T))
  }else(study_period)
  x <- x[y %in% study_period]
  x[, m := lubridate::month(date)]
  x[, d := lubridate::day(date)]
  x$week.number <- strftime(x$date, format = "%V")
  x[, quarter := ifelse(m %in% c(1,2,3), 'Q1',
                        ifelse(m %in% c(4,5,6), 'Q2',
                               ifelse(m %in% c(7,8,9), 'Q3',
                                      'Q4')))]
  x$quarter <- factor(x$quarter, levels= c('Q1','Q2','Q3','Q4'))
  x[, time.bc := NULL]

  ## Position of the hauls #----
  ## Each haul position is averaged so that there is only one position per haul.
  ####  midPoint function from the geosphere package
  p1 <- matrix(c(x$haul.lon.start * pi/180,
                 x$haul.lat.start * pi/180),
               ncol=2 )
  p2 <- matrix(c(x$haul.lon.stop * pi/180,
                 x$haul.lat.stop * pi/180),
               ncol=2 )
  midhaul <- as.data.frame(geosphere::midPoint(p1, p2))
  midhaul$lon <- midhaul$lon * 180/pi # back to degrees
  midhaul$lat <- midhaul$lat * 180/pi # back to degrees
  names(midhaul)[1] <- "lon.haul"
  names(midhaul)[2] <- "lat.haul"
  x <- cbind(x, midhaul)
  rm(list = c("midhaul", "p1", "p2"))

  ## Make sure dataframe is organized the way we want
  x <- dplyr::arrange(x, rnum)

  ## Soak times #----
  ### List the hauls with manually determined soak times
  soak_files <- base::list.files(path_to_soak,
                                 pattern = "*csv",
                                 full.names = TRUE,
                                 recursive = FALSE)
  list_soak <- base::lapply(soak_files,
                            utils::read.csv,
                            ## Uncomment below if the format looks weird (and comment the line above)
                            # utils::read.csv, sep = ",",
                            header=TRUE,
                            stringsAsFactors = FALSE,
                            quote = "")
  soakdata <- data.table::rbindlist(list_soak,fill=TRUE)
  soakdata <- soakdata[, c('IDhaul','soak')]
  soakdata$IDhaul <- as.factor(soakdata$IDhaul)
  soakdata$soak <- as.numeric(soakdata$soak)
  soakdata <- data.table::setkey(soakdata,IDhaul)
  soakdata <- soakdata[IDhaul!='']
  x <- data.table::setkey(x,IDhaul)
  x <- soakdata[x, on = .(IDhaul)]
  x[is.na(soak), soak:=i.soak] # only replace the value missing from left table
  x[,"i.soak":=NULL]
  ## Manual BB bug fix (when it rarely rounds down to 0 hour)
  x[, soak := data.table::fifelse(soak == 0, 1, soak)]

  ## Standardised effort #----
  ### as net length (in km) multiplied by soak time (hours): km*hour
  x$std_effort <- as.numeric(x$soak * x$netlength / 1000) # netlength is in m

  if (give_me_more == TRUE){
    ## Add info on depth (m) at point #----

    ### First, we can recycle the work what we have done previously and update
    ### only the rows for which we do not have depth and d2shore already
    old.x <- data.table::fread(path.to.old.notes)
    old.x <- subset(old.x, select = c('IDevent','depth','d2shore'))
    x <- merge(x, old.x, by = "IDevent", all.x = TRUE)

    ## Now we can update the values that are missing (i.e. NA in depth and d2shore)
    x.tmp <- x[ is.na(depth) | is.na(d2shore) ]
    x.tmp <- x.tmp[ !is.na(lon.haul) & !is.na(lat.haul) ] ## Needed to estimate depth and d2shore
    x.tmp <- x.tmp  %>%
      # sf::st_as_sf(coords = c('lon.haul','lat.haul'), na.fail = FALSE,
      #              crs = 4326) %>%
      # sf::st_transform(3035) %>%
      dplyr::select(lon.haul, lat.haul, IDhaul, depth, d2shore) %>%
      dplyr::distinct()

    if( length(x.tmp$depth) > 0 ){

      ### Define the ERDDAP dataset ID and the variable you want to retrieve
      #### https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/cf51df64-56f9-4a99-b1aa-36b8d7b743a1
      dataset_id <- "bathymetry_dtm_2024"
      variable <- "elevation"
      erddap_url <- "https://erddap.emodnet.eu/erddap/"
      out <- rerddap::info(datasetid = dataset_id,
                           url = erddap_url)
      depth_EMODNET <- function(lat, lon) {

        if ( !is.na(lat) &
             !is.na(lon) &
             lat > 15.000520833333333 &
             lat < 89.99947916660017 &
             lon > -35.99947916666667 &
             lon < 42.99947916663591){
          # Query the ERDDAP server
          Sys.sleep(0.1)
          result <- suppressMessages(
            rerddap::griddap(
              out,
              fields = variable,
              latitude = c(lat,lat),
              longitude = c(lon,lon))
          )

          # Extract the depth value
          depth <- result$data[[variable]]

        } else(
          depth <- NA_integer_
        )

        return(depth)
      }

      chunk_size <- 500
      num_chunks <- ceiling(nrow(x.tmp) / chunk_size)
      depth_in_chuncks <- list()

      for (i in 1:num_chunks) {
        # Calculate the start and end indices for the current chunk
        start_idx <- (i - 1) * chunk_size + 1
        end_idx <- min(i * chunk_size, nrow(x.tmp))

        # Extract the current chunk of data
        x_chunk <- x[start_idx:end_idx, ]

        # Query the server with the current chunk
        one_chunck <-  mapply(depth_EMODNET,
                              x_chunk$lat.haul,
                              x_chunk$lon.haul)
        # Store the result
        depth_in_chuncks[[i]] <- one_chunck

        # Print progress
        cat("Processed chunk", i, "of", num_chunks, "\n")
        Sys.sleep(1)

      }
      x.tmp$depth <- unlist(depth_in_chuncks)

      ## For the points that are not found, we can use a backup plan:
      depth.ras.dk <- terra::rast(x = path.to.raster)
      x.tmp.sub <- x.tmp[is.na('depth'),]
      if ( length(x.tmp.sub$depth) > 0){

        if( "lon.haul" %in% names(x.tmp.sub) ){
          dk.sppts <- sf::st_as_sf(x.tmp.sub,
                                   coords = c('lon.haul','lat.haul'),
                                   na.fail = FALSE) } else {
                                     dk.sppts <- sf::st_as_sf(x.tmp.sub,
                                                              coords = c('lon','lat'),
                                                              na.fail = FALSE)
                                   }
        depth.dk.df <- (terra::extract(x = depth.ras.dk,
                                       y = dk.sppts,
                                       df = TRUE))$D5_2020
        for (i in 1: length(x.tmp.sub$depth)){
          if( is.na(x.tmp.sub$depth[i]) ){
            x.tmp.sub$depth[i] <- depth.dk.df[i] }
        }
        x.tmp.sub <- x.tmp.sub[, depth := data.table::fifelse(depth>0, -2, depth)]

        x.tmp <- x.tmp[subset(x.tmp.sub,
                              select = c('IDhaul', 'depth')),
                       depth := i.depth]

      }

      ## Add info on distance (m) to nearest point on shore #----
      coastline <- sf::st_read(path.to.coastline)
      x_sf <- x.tmp  %>%
        sf::st_as_sf(coords = c('lon.haul','lat.haul'), na.fail = FALSE,
                     crs = 4326) %>%
        sf::st_transform(3035)
      distances <- sapply(1:nrow(x_sf), function(i) {
        point <- x_sf[i, ]
        min(sf::st_distance(point, coastline))
      })
      x.tmp$d2shore <- distances

      ## In the map we use here, there are a couple a islets in the Sound that
      ## do not appear to be correct. As a result, we could rarely have d2shore=0
      ## Fix by forcing a minimum d2shore of 20 metres
      x.tmp <- x.tmp %>%
        dplyr::mutate(d2shore = ifelse(d2shore == 0, 20, d2shore))

      x[x.tmp, `:=`(
        depth = data.table::fcoalesce(depth, i.depth),
        d2shore = data.table::fcoalesce(d2shore, i.d2shore)
      ), on = "IDhaul"]

    }

    ## Add variable ICES area, ICES rectangle and ICES subrectangle #----
    x.sub <- subset(x, select = c('lon.haul','lat.haul','IDhaul')) %>%
      dplyr::distinct()

    x.sub <- ggleR::pts.ices.area(x.sub)

    dk.sppts <- sf::st_as_sf(x.sub,
                             coords = c('lon.haul','lat.haul'),
                             na.fail = FALSE)
    suppressWarnings(
      x.sub[, icesrect:= data.table::fifelse(!is.na(lon.haul) & !is.na(lat.haul),
                                         mapplots::ices.rect2(lon.haul,
                                                              lat.haul),
                                         NA_character_)])
    suppressWarnings(
      x.sub <- x.sub %>%
        dplyr::rowwise() %>%
        dplyr::mutate(subrect = dplyr::if_else(!is.na(icesrect),
                                               ggleR::ices.subrect(lon.haul,
                                                                   lat.haul),
                                               NA)))

    x <- merge(x,
               subset(x.sub, select = c('ices.area','icesrect','subrect',
                                        'IDhaul')),
               by = 'IDhaul')

    return(x)
  }
  return(x)
}
