
#' Find the nearest stations to a given longitude and latitude
#' 
#' Find the nearest stations to a given longitude and latitude
#'
#' @param lon Longitude.
#' @param lat Latitude.
#'
#' @author Fiona Evans
#' 
#' @return Returns a vector of station ids, in order of increasing distance.
#' @export
nearestStations <- function(lon, lat) {

  d <- function(x1, y1, x2, y2) { sqrt((x1-x2)^2 + (y1-y2)^2) }
  
  dmat <- rep(NA, nrow(STATIONS))
  for (i in 1:nrow(STATIONS)) dmat[i] <- d(lon, lat, STATIONS$LONGITUDE[i], STATIONS$LATITUDE[i])
  o <- order(dmat)
  
  id <- STATIONS[o, "SITE_NO"]
  data.frame(id=id, distance=dmat[o])
  
}
