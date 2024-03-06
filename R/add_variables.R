#' Add variables to the extracted EM dataset
#' Take in the BlackBox data from the BBimport function and adds a number of important parameters
#' @param x Data output from function BBimport
#' @param give_me_more Should optional variables not relevant for RDBES be created too (default is TRUE)?
#' @return Same dataste with additional columns
#' @export
add_variables <- function(x = data_work, give_me_more = give_me_more) {
  data_work <- y <- m <- d <- quarter <- lat.start <- lat.stop <- lon.start <- lon.stop <- rnum <- NULL
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
  x$date <- lubridate::ymd(x$date)
  x[, y := lubridate::year(date)]
  x[, m := lubridate::month(date)]
  x[, d := lubridate::day(date)]
  x$week.number <- strftime(x$date, format = "%V")
  x[, quarter := ifelse(m %in% c(1,2,3), 'Q1',
                        ifelse(m %in% c(4,5,6), 'Q2',
                               ifelse(m %in% c(7,8,9), 'Q3',
                                      'Q4')))]
  x$quarter <- factor(x$quarter, levels= c('Q1','Q2','Q3','Q4'))

  x <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(lat.haul = sum(lat.start,lat.stop)/2,
                  lon.haul = sum(lon.start,lon.stop)/2) %>%
    data.table::as.data.table(.) # print as dt, not tibble

  ## Make sure dataframe is organized the way we want
  x <- dplyr::arrange(x, rnum)

  ## Standardised effort
  ## as net length (in km) multiplied by soak time (hours): km*hour
  x$std_effort <- as.numeric(x$soak * x$netlength / 1000) # netlength is in meters

  if (give_me_more == TRUE){
    x <- ggleR::get.depth(x)
    coastline <- sf::st_read("Q:/scientific-projects/cctv-monitoring/data/GIS/",
                             "coastline")

    x_sf <- tidyr::drop_na(x, 'date')  %>%
      sf::st_as_sf(coords = c('lon.haul','lat.haul'), na.fail = FALSE) %>%
      sf::st_set_crs(4326)

    ### And then project:
    x_sf <- x_sf %>% sf::st_transform(32632)
    ## Calculate the distance between each obs. and the closest coast, by:
    dist <- sf::st_distance(x_sf, coastline[1:53,])

    ## Store the results
    x$d2shore <- as.numeric(apply(dist, 1, min))
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ICES area
    x <- ggleR::pts.ices.area(x)
    return(x)
  }
  return(x)
}
