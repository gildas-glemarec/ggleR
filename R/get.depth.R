#' Extract depth from pre-made raster file
#' This scripts extracts the depth (metres) for each point in a dataset
#' It also adds the ICES subrectangle these points are in
#' @param x dataset with geographic coordinates in decimal as lon/lat
#' @param path.to.raster Path to the a raster file with depth info
#' @return Same dataset with added variables
#' @export
get.depth <- function(x,
                      path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/D5_2020.tif"){
  lon.haul <- lat.haul <- depth <- icesrect <-
depth.ras.dk <- terra::rast(x = path.to.raster)
dk.sppts <- sf::st_as_sf(x, coords = c('lon.haul','lat.haul'), na.fail = FALSE)
## Add the variable ICES rectangle and ICES subRectangle #----
x <- x %>%
  dplyr::mutate(icesrect = dplyr::if_else(!is.na(lon.haul) & !is.na(lat.haul),
                                   mapplots::ices.rect2(lon.haul, lat.haul),
                                   NA))
ices.subrect <- function (lon, lat)
{
  x <- floor(lon + 60) + 1000
  y <- floor(lat * 2) - 71 + 100
  num1 <- substr(y, 2, 3)
  lett <- LETTERS[as.numeric(substr(x, 2, 3))]
  num2 <- substr(x, 4, 4)
  res <- paste(num1, lett, num2, sep = "")
  centroid.ices.rect <- mapplots::ices.rect(res)
  num3 <- lon - centroid.ices.rect[,1]
  num4 <- lat - centroid.ices.rect[,2]
  X <- data.table::data.table(res, centroid.ices.rect, num3, num4)

  i <- function(if_stat, then) {
    if_stat <- lazyeval::expr_text(if_stat)
    then    <- lazyeval::expr_text(then)
    sprintf("ifelse(%s, %s, ", if_stat, then)
  }
  e <- function(else_ret) {
    else_ret <- lazyeval::expr_text(else_ret)
    else_ret
  }
  ie <- function(...) {
    args <- list(...)

    for (i in 1:(length(args) - 1) ) {
      if (substr(args[[i]], 1, 6) != "ifelse") {
        stop("All but the last argument, need to be i functions.", call. = FALSE)
      }
    }
    if (substr(args[[length(args)]], 1, 6) == "ifelse"){
      stop("Last argument needs to be an e function.", call. = FALSE)
    }
    args$final <- paste(rep(')', length(args) - 1), collapse = '')
    eval_string <- do.call('paste', args)
    eval(parse(text = eval_string))
  }
  X$subRect <-
    ie(
      i(num3 <= -0.08333333 & num4 >= 0.16666667, paste(res, 1, sep = "")),
      i(num3 >= 0.08333333 & num4 >= 0.16666667, paste(res, 7, sep = "")),
      i(num3 <= -0.08333333 && num4 <= -0.16666667, paste(res, 3, sep = "")),
      i(num3 >= 0.08333333 && num4 <= -0.16666667, paste(res, 9, sep = "")),
      i(dplyr::between(num3, -0.08333333, 0.08333333) && num4 <= -0.16666667, paste(res, 6, sep = "")),
      i(dplyr::between(num3, -0.08333333, 0.08333333) && num4 >= 0.16666667, paste(res, 4, sep = "")),
      i(num3 >= 0.08333333 && dplyr::between(num4, -0.16666667, 0.16666667), paste(res, 8, sep = "")),
      i(num3 <= -0.08333333 && dplyr::between(num4, -0.16666667, 0.16666667), paste(res, 2, sep = "")),
      e(paste(res, 5, sep = ""))
    )
  return(X$subRect)
}
suppressWarnings(
  x <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(subrect = dplyr::if_else(!is.na(icesrect),
                                    ices.subrect(lon.haul,lat.haul),
                                    NA))
)

depth.dk.df <- (terra::extract(x = depth.ras.dk,
                               y = dk.sppts,
                               df = TRUE))$D5_2020
x <- data.table::data.table(x)[, depth:= depth.dk.df]
x <- x[, depth := data.table::fifelse(depth>0, -2, depth)]
return(x)
gc()
}
