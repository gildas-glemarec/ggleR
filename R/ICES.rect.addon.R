## Simple function to create a table with ices.rect + lat/lon of the centroid
library(mapplots)
library(sf)
library(data.table)
# library(emodnet.wfs)
# wfs_bath <- emodnet_init_wfs_client(service = "bathymetry")
# emodnet_get_layers(wfs = wfs_bath, layers = layers, simplify = TRUE)
#
# ices.rectangles <- read_sf('H:/c-users/Maps/Master layers/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp')
# setDT(ices.rectangles, key = 'ICESNAME')
# ##location as centroid of ICES stat. rect.
# ices.rectangles[, lon := mapplots::ices.rect(ices.rectangles$ICESNAME)[,1]]
# ices.rectangles[, lat := mapplots::ices.rect(ices.rectangles$ICESNAME)[,2]]
# ## Distance to shore (in meters) ####
# coastline <- sf::st_read("Q:/scientific-projects/cctv-monitoring/data/GIS/",
#                          "coastline")
# coastline <- coastline[!is.na(coastline$NAME),]
#
# ## Remake it an sf object
# ices.rectangles <- ices.rectangles  %>%
#   st_as_sf(coords = c('lon','lat')) %>%
#   st_set_crs(4326)
# ### And then project:
# ices.rectangles <- ices.rectangles %>% sf::st_transform(32632)
# ## Calculate the distance between each obs. and the closest coast, by:
# dist <- st_distance(ices.rectangles, coastline)
# ## Store the results
# ices.rectangles$d2shore <- as.numeric(apply(dist, 1, min))
# ## Calculate depth and distance to shore of the ICES rect centroids
# get.depth <- function(x,
#                       path.to.raster = "Q:/scientific-projects/cctv-monitoring/data/GIS/alldepth.tif"){
#   ## Depth at point
#   depth.ras.dk <- terra::rast(x = path.to.raster)
#   depth.dk.df <- (terra::extract(x = depth.ras.dk,
#                                  y = x,
#                                  df = TRUE))$alldepth
#   x <- data.table::data.table(x)[, depth:= depth.dk.df]
#   x <- x[, depth := data.table::fifelse(depth>0, -2, depth)]
#   return(x)
#   gc()
# }
# ices.rectangles <- get.depth(ices.rectangles,
#                              path.to.raster = 'Q:/scientific-projects/cctv-monitoring/data/GIS/alldepth.tif')
# ices.rectangles <- ices.rectangles[!is.na(depth)]
# ices.rectangles <- ices.rectangles[depth<0]

ices.rectangles <- read_sf('H:/c-users/Maps/DepthDK/ICES rect depth.gpkg')
ices.rectangles <- ices.rectangles %>% sf::st_transform(32632)
## Distance to shore (in meters) ####
coastline <- sf::st_read("Q:/scientific-projects/cctv-monitoring/data/GIS/",
                         "coastline")
coastline <- coastline[!is.na(coastline$NAME),]
## Calculate the distance between each obs. and the closest coast, by:
dist <- st_distance(ices.rectangles, coastline)
## Store the results
ices.rectangles$d2shore <- as.numeric(apply(dist, 1, min))
ices.rectangles$depth <- as.numeric(ices.rectangles$`_mean`)

saveRDS(ices.rectangles, 'H:/c-users/Maps/ICES_rect.RDS')
