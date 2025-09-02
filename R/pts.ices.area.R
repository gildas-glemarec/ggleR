#' Finds the ICES area a geographic point falls into
#' @param x dataset with geographic coordinates in decimal as lon/lat
#' @param dsn Path to the a directory with ICES shapefiles
#' @param layer name of the layer to use
#' @return Same dataset with added variables
#' @export
pts.ices.area <- function(x,
                          dsn = "Q:/20-forskning/12-gis/Dynamisk/GEOdata/BasicLayers/Boundaries/Ices/ICES_areas",
                          layer = "ICES_Areas_20160601_cut_dense_3857"){
  ICES.areaSF <- sf::st_read(dsn = dsn,
                             layer = layer)
  x$lon <- x$lon.haul
  x$lat <- x$lat.haul
  x.sp <- sf::st_as_sf(x, coords = c(x = "lon", y = "lat"), na.fail = FALSE,
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
  ices.area <- sf::st_transform(ICES.areaSF, sf::st_crs(x.sp))

  sf::sf_use_s2(FALSE)
  x <- sf::st_join(x = x.sp, y = ices.area)

  data.table::setDT(x, key = 'IDhaul')
  data.table::setnames(x = x, old = 'Area_27', new = 'ices.area')
  x$ices.area <- factor(x$ices.area)
  x$ices.area <- forcats::fct_na_value_to_level(x$ices.area)
  return(x)}
