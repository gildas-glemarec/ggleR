#' Obtain the depth at a point (lat/lon) from EMODnet Digital Bathymetry (DTM 2024) and using RERDDAP
#' This scripts extracts the depth (metres) for each point in a dataset. Missing values are NA_numeric_
#' @param lat decimal latitude
#' @param lon decimal longitude
#' @return A vector (numeric) of depth values in metre
#' @export
get_depth_EMODNET <- function(lat, lon) {

  # Define the ERDDAP dataset ID and the variable you want to retrieve
  #### https://emodnet.ec.europa.eu/geonetwork/srv/eng/catalog.search#/metadata/cf51df64-56f9-4a99-b1aa-36b8d7b743a1
  dataset_id <- "bathymetry_dtm_2024"
  variable <- "elevation"
  erddap_url <- "https://erddap.emodnet.eu/erddap/"
  if ( !is.na(lat) &
       !is.na(lon) &
       lat > 15.000520833333333 &
       lat < 89.99947916660017 &
       lon > -35.99947916666667 &
       lon < 42.99947916663591){
    # Query the ERDDAP server
    result <- suppressMessages(
      rerddap::griddap(
        rerddap::info(datasetid = dataset_id,
                      url = erddap_url),
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
