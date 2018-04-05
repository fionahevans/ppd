#' Download PPD data and store climatology
#' 
#' Download PPD data for a station and store climatology as a data frame where the 3rd dimension index by year
#'
#' @param id Weather station ID.
#' @param start.year Start year.
#' @param end.year End year.
#' @param username SILO username (available from https://legacy.longpaddock.qld.gov.au/silo/).
#' @param password SILO password (available form https://legacy.longpaddock.qld.gov.au/silo/).
#'
#' @author Fiona Evans
#'
#' @return Data frame containing daily weather data (leap year days omitted).
#' @export
getClimatology <- function(id, start.year, end.year, username, password) {
  d <- getPPD(id, paste0(start.year, "0101"), 
              paste0(end.year, "1231"), username, password)
  
  d$Year <- as.numeric(format(d$Date, "%Y"))
  
  a <- array(dim=c(365, ncol(d), length(start.year:end.year)))
  dimnames(a) <- list(c(1:365), dimnames(d)[[2]], c(start.year:end.year))
  indx <- 1
        
  for (y in start.year:end.year) {
    d1 <- subset(d, Year == y)
    if (is.leapyear(y)) {
      d1 <- subset(d1, Date != as.Date(paste0(y, "0229"), "%Y%m%d"))
    }
    a[,,indx] <- as.matrix(d1)
    indx <- indx + 1
  }
   
  a
}



is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}