## Simple function to list ICES rectangles in the Baltic + Greater North Sea
## with mean distance to shore and depth
library(mapplots)
library(sf)
library(data.table)
icesrect_d2shore <- readRDS('H:/c-users/Maps/icesrect_d2shore.rds')
icesrect_d2shore <- as.data.table(icesrect_d2shore)
icesrect_d2shore <- icesrect_d2shore[, geom := NULL]
icesrect_depth <- st_read('H:/c-users/Maps/Master layers/ICES rect mean depth EMODnet NSAtl.gpkg')
ices.rectangles <- merge(icesrect_depth, icesrect_d2shore, all.x = TRUE, by = 'ICESNAME')
# ## Plot mean d2shore and mean depth of the squares
# plot(ices.rectangles["mean.d2shore"])
# plot(ices.rectangles["X_mean"])
## Remove the ICES rect for which we have no depth (outside our area of interest)
ices.rectangles <- subset(ices.rectangles, !is.na(X_mean))
ices.rectangles <- ices.rectangles %>%
  dplyr::rename(depth.mean=X_mean) %>%
  dplyr::rename(depth.median=X_median) %>%
  dplyr::rename(depth.stdev=X_stdev) %>%
  dplyr::rename(depth.min=X_min) %>%
  dplyr::rename(depth.max=X_max) %>%
  dplyr::rename(d2shore.mean=mean.d2shore) %>%
  dplyr::select(c(-ICESNAME_1,-ICESNAME_2))
names(ices.rectangles)
st_write(ices.rectangles,
         'H:/c-users/Maps/Master layers/ices.rectangles_meandepth_meand2shore.gpkg')
