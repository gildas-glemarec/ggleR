#' Add variables to the extracted EM dataset
#' Take in the Black Box data from the BBimport function and adds a number of important parameters
#' @param x Data output from function BBimport
#' @param give_me_more Should optional variables not relevant for RDBES be created too (default is TRUE)?
#' @param path_to_soak Some soak time info is missing (not found in BB). Manual fix based on a prior extraction (input is a csv file)
#' @param path.to.coastline ESRI Shapefile of the region (used to estimate distance to shore). By default, the file is Europe (Bounding box:  xmin: 943609.8 ymin: -375446 xmax: 7601958 ymax: 6825119; Projected CRS: ETRS89-extended / LAEA Europe)
#' @return Same dataset with additional columns
#' @export
add_variables <- function(x = data_work, give_me_more = give_me_more,
                          path_to_soak = path_to_soak,
                          path.to.coastline = path.to.coastline) {
  . <- data_work <- y <- m <- d <- quarter <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL
  x$rnum <- as.integer(row.names(x))
  data.table::setDT(x,'rnum')
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
  x[, m := lubridate::month(date)]
  x[, d := lubridate::day(date)]
  x$week.number <- strftime(x$date, format = "%V")
  x[, quarter := ifelse(m %in% c(1,2,3), 'Q1',
                        ifelse(m %in% c(4,5,6), 'Q2',
                               ifelse(m %in% c(7,8,9), 'Q3',
                                      'Q4')))]
  x$quarter <- factor(x$quarter, levels= c('Q1','Q2','Q3','Q4'))
  x[, time.bc := NULL]
  x <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(lat.haul = sum(lat.start,lat.stop)/2,
                  lon.haul = sum(lon.start,lon.stop)/2) %>%
    data.table::as.data.table(.) # print as dt, not tibble

  ## Make sure dataframe is organized the way we want
  x <- dplyr::arrange(x, rnum)

  ## Soak times
  ### List the hauls with manually determined soak times
  soak_files <- list.files(path_to_soak,
                           pattern = "*csv",
                           full.names = TRUE,
                           recursive = FALSE)
  list_soak <- lapply(soak_files,
                      utils::read.csv2,
                      ## Uncomment below if the format looks weird (and comment the line above)
                      # utils::read.csv, sep = ",",
                      header=TRUE,
                      stringsAsFactors = FALSE,
                      quote = "")
  soakdata <- data.table::rbindlist(list_soak,fill=TRUE)
  soakdata <- soakdata[, c('IDhaul','soak')]
  soakdata$IDhaul <- as.factor(soakdata$IDhaul)
  soakdata <- data.table::setkey(soakdata,IDhaul)
  x <- data.table::setkey(x,IDhaul)
  x <- soakdata[rem_data_from2021, on = .(IDhaul)]
  x[is.na(soak), soak:=i.soak] # only replace the value missing from left table
  x[,"i.soak":=NULL]

  ## Standardised effort
  ### as net length (in km) multiplied by soak time (hours): km*hour
  x$std_effort <- as.numeric(x$soak * x$netlength / 1000) # netlength is in m

  if (give_me_more == TRUE){
    ## Add info on depth (m) at point
    x <- ggleR::get.depth(x)
    ## Add info on distance (m) to nearest point on shore
    coastline <- sf::st_read(path.to.coastline
      # "Q:/gis/Dynamisk/GEOdata2020/BasicLayers/Coastlines/Europe/EEA Europe/EEA_Coastline_20170228.shp"
      )
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

    # # ## Older version (coastline shapefile incomplete with missing islets)
    # # coastline <- sf::st_read("Q:/scientific-projects/cctv-monitoring/data/GIS/",
    # #                         "coastline")
    # # x_sf <- tidyr::drop_na(x, 'date')  %>%
    # #  sf::st_as_sf(coords = c('lon.haul','lat.haul'), na.fail = FALSE) %>%
    # #  sf::st_set_crs(4326)
    # # ### And then project:
    # # x_sf <- x_sf %>% sf::st_transform(32632)
    # # ## Calculate the distance between each obs. and the closest coast, by:
    # # dist <- sf::st_distance(x_sf, coastline[1:53,])
    # # ## Store the results
    # # x$d2shore <- as.numeric(apply(dist, 1, min))


    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ICES area
    x <- ggleR::pts.ices.area(x)
    return(x)
  }
  return(x)
}
