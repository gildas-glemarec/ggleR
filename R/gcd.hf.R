#' Radial distance between spatial points
#' @param long1 longitude start (decimal)
#' @param long2 longitude end (decimal)
#' @param lat1 latitude start (decimal)
#' @param lat2 latitude end (decimal)
#' @return distance in metres (as.numeric)
#' @export
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371000 # Earth mean radius [m]
  long1 <- long1 * pi/180
  lat1 <- lat1 * pi/180
  long2 <- long2 * pi/180
  lat2 <- lat2 * pi/180
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in m
}
