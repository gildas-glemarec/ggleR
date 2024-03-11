#' Extract depth from pre-made raster file
#' This scripts extracts the depth (metres) for each point in a dataset.
#' It also adds the ICES subrectangle these points are in.
#' @param x dataset with geographic coordinates in decimal as lon/lat
#' @param path.to.raster Path to the a raster file with depth info
#' @return Same dataset with added variables
#' @export
get.depth <- function(x,
                      path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/D5_2020.tif"){
  lon.haul <- lat.haul <- lon <- lat <- depth <- icesrect <- NULL

  depth.ras.dk <- terra::rast(x = path.to.raster)
  x <- data.table::as.data.table(x)

  if("lon.haul" %in% names(x)){
    dk.sppts <- sf::st_as_sf(x, coords = c('lon.haul','lat.haul'), na.fail = FALSE)
    ## Add the variable ICES rectangle and ICES subRectangle #----
    x[, icesrect:= data.table::fifelse(!is.na(lon.haul) & !is.na(lat.haul),
                           mapplots::ices.rect2(lon.haul, lat.haul),
                           NA_character_)]
    suppressWarnings(
      x <- x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(subrect = dplyr::if_else(!is.na(icesrect),
                                               ggleR::ices.subrect(lon.haul,lat.haul),
                                               NA)))
    } else {dk.sppts <- sf::st_as_sf(x, coords = c('lon','lat'), na.fail = FALSE)
           ## Add the variable ICES rectangle and ICES subRectangle #----
           x[, icesrect:= data.table::fifelse(!is.na(lon) & !is.na(lat),
                                  mapplots::ices.rect2(lon, lat),
                                  NA_character_)]
    suppressWarnings(
      x <- x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(subrect = dplyr::if_else(!is.na(icesrect),
                                               ggleR::ices.subrect(lon,lat),
                                               NA)))
    }

  depth.dk.df <- (terra::extract(x = depth.ras.dk,
                                 y = dk.sppts,
                                 df = TRUE))$D5_2020
  x <- data.table::data.table(x)[, depth:= depth.dk.df]
  x <- x[, depth := data.table::fifelse(depth>0, -2, depth)]
  return(x)
  gc()
}
