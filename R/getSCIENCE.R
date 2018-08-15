

#' Download list of DPIRD SCIENCE weather stations
#' 
#' Download list of DPIRD SCIENCE weather stations - contains Patched Poitn and DPIRD
#' automatic weather stations
#'
#' @param apiKey API key (see https://www.agric.wa.gov.au/science-api-20).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing list of weather stations and metadata..
#' @export
getSCIENCEstations <- function(apiKey){
  g <- url(paste0("https://api.dpird.wa.gov.au/v2/science/stations.json?api_key=", apiKey))
  
  res <- fromJSON(g)
  
  stations <- res$collection
  
  stations$latitude <- as.numeric(stations$latitude)
  stations$longitude <- as.numeric(stations$longitude)
  for (i in 1:nrow(stations)) stations$stationName[i] <- simpleCap(stations$stationName[i])
  stations$links <- NULL
 
  return(stations)
}

simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#' Download  current 'rainfall to date' data using DPIRD SCIENCE API
#' 
#' Download current 'rainfall to date' data using DPIRD SCIENCE API
#'
#' @param id Weather station ID.
#' @param apiKey API key (see https://www.agric.wa.gov.au/science-api-20).
#'
#' @author Fiona Evans
#' 
#' @return List of data frames.
#' @export
getRTD <- function(stations.id, apiKey){
  
  
  today <- Sys.time()
  year <- as.numeric(format(today, "%Y"))

  summerStartDate <- paste0(year-1, "-11-01")
  growingSeasonStartDate <- paste0(year, "-04-01")
  growingSeasonEndDate <- paste0(year, "-10-31")
  forecastDate <- format(today, "%Y-%m-%d")
    
  url <- paste0(science, station.id, 
                  "?forecastDate=", forecastDate,
                  "&summerStartDate=", summerStartDate,
                  "&growingSeasonStartDate=", growingSeasonStartDate,
                  "&growingSeasonEndDate=", growingSeasonEndDate,
                  "&api_key=", apiKey)
    
  res <- fromJSON(url(url))
    
  rain <- res$data$currentSeasonalRainfall
  rain$date <- as.Date(rain$date, "%Y-%m-%d")
    
  proj <- proj2df(res$data$projectedSeasonalRainfall)
  hist <- hist2df(res$data$historicalRainfall)
    
  return(list(rain=rain, hist=hist, proj=proj))
}

proj2df <- function(tmp) {
  dec <- matrix(ncol=9, nrow=length(tmp$deciles)) 
  for (i in 1:nrow(dec)) {
    dec[i, ] <- tmp$deciles[[i]][, 2]
  }
  dec <- as.data.frame(dec)
  names(dec) <- paste0("decile", c(1:9))
  dec$date <- as.Date(tmp$date, "%Y-%m-%d")
  dec
}

hist2df <- function(tmp) {
  dec <- matrix(ncol=9, nrow=length(tmp$deciles)) 
  for (i in 1:nrow(dec)) {
    dec[i, ] <- tmp$deciles[[i]][, 2]
  }
  dec <- as.data.frame(dec)
  names(dec) <- paste0("decile", c(1:9))
  dec$date <- as.Date(paste0(year, "-", tmp$date), "%Y-%m-%d")
  dec
}