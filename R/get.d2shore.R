#' Calculates the distance to the nearest point on shore
#' This scripts estimates the distance to shore (d2shore) in metres for each row in the input dataset, assuming ESPG:4326. Missing values are NA_numeric_
#' If no shapefile is input, Europe is used as default (with ESPG:3035)
#' @param x data.frame or data.table object with coordinates as 'LE_LON' and 'LE_LAT' in decimal
#' @param shapefile ShapeFile to use - Europe as default
#' @param crs_src CRS of the points in the dataset (deafaults to 4326)
#' @return A vector (numeric) of depth values in metre
#' @export
get_d2shore <- function(x = x,
                        shapefile = NULL,
                        crs_src = 4326) {

  if( is.null(shapefile) ){
    ## If there is no shapefile called "coastline", then download it
    zip_url <- "https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon/at_download/file.zip"
    ## Create a temporary directory
    temp_dir <- tempdir()
    ## Define the path for the downloaded zip file
    zip_file_path <- file.path(temp_dir, "data.zip")
    ## Download the zip file
    utils::download.file(zip_url, zip_file_path)
    ## Unzip the file
    utils::unzip(zip_file_path, exdir = temp_dir)
    ## List the files in the temporary directory
    unzipped_files <- list.files(temp_dir)
    ## Read the shapefile
    file_path <- file.path(temp_dir, unzipped_files)
    shp_file <- grep(".shp$", file_path, value = TRUE)
    shapefile <- sf::st_read(shp_file)}

  crs_shp <- sf::st_crs(shapefile)

  x_sf <- sf::st_as_sf(x,
                       coords = c('LE_LON','LE_LAT'),
                       na.fail = FALSE,
                       crs = crs_src) |>
    sf::st_transform(crs_shp)

  distances <- sapply(1:nrow(x_sf), function(i) {
    point <- x_sf[i, ]
    min(sf::st_distance(point, shapefile))
  })

  return(distances)
}
