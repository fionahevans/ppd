

#' Download list of DPIRD automatic weather stations
#' 
#' Download list of DPIRD automatic weather stations
#'
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing list oif weather stations and metadata..
#' @export
getDPIRDstations <- function(apiKey){
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations.json?api_key=", 
                           apiKey)))
  
  stations <- g$result
  
  stations$latitude <- as.numeric(stations$latitude)
  stations$longitude <- as.numeric(stations$longitude)
  stations$start_date <- as.Date(stations$start_date, "%Y-%m-%d")
  
  return(stations)
}




#' Download DPIRD daily weather data for one year (may contain missing values)
#' 
#' Download DPIRD daily weather data for one year (may contain missing values).
#'
#' @param id Weather station ID.
#' @param year Year (numeric YYYY).
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getDPIRDdaily <- function(id, year, apiKey){
  
  from <- paste0(year, "-01-01")
  to <- paste0(year, "-12-31")
    
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/dailysummary.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey)))
  result <- g$result
  
  if (length(result) == 0) {
    result <- NULL
    print(paste("No data available for station id", id, "in year", year, ".\n"))
    print("Use getDPIRDstations() to check station start dates.")
  }
  
  if (length(result) > 0){
    for (j in c(3,5,7,8,10,12,13,14,16,18,19,20,21,22,23,25,27,28,30,
              33,35,37,38,40,42,43)) result[, j] <- as.numeric(result[,j])
  
  result$year <- as.numeric(substr(result$record_date, 1, 4))
  result$month <- as.numeric(substr(result$record_date, 6, 7))
  result$day <- as.numeric(substr(result$record_date, 9, 10))
  
  result$date <- as.Date(result$record_date, "%Y-%m-%d")
  
  }
  
  return(result)
}

is.leapyear <- function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

distance <- function (x1, y1, x2, y2) {return(sqrt((x1-x2)^2 + (y1-y2)^2))}

#' Fill missing DPIRD daily weather data (for one year) with data from nearest PPD station.
#' 
#' Fill missing DPIRD daily weather data (for one year) with data from nearest PPD station.
#'
#' @param id Weather station ID.
#' @param year Year.
#' @param weather data frame containing DPIRD weather data downloaded using getDPIRDdaily().
#' @param silo.apiKey SILO API key (available from https://silo.longpaddock.qld.gov.au/).
#' 
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing filled daily weather data.
#' @export
fillDPIRDdaily <- function(id, year, weather, silo.apiKey){
  
  leap <- is.leapyear(year)
  
  if (nrow(weather) < as.numeric(difftime(weather$record_date[nrow(weather)], 
                                          weather$record_date[1]))){
  
  #if ((!leap && nrow(weather) < 365) || (leap && nrow(weather) < 366)) {
    # Need to fill data
    
    # Find nearest PPD station
    i <- which(stations$station_id == id)
    lat <- stations[i, "latitude"]
    lon <- stations[i, "longitude"]
    d <- rep(NA, nrow(STATIONS))
    for (j in 1:nrow(STATIONS)) d[j] <- distance(lon, lat, STATIONS[j, "LONGITUDE"], STATIONS[j, "LATITUDE"])
    ppd.id <- STATIONS[which(d == min(d)), "SITE_NO"]
    start <- format(weather[1, "date"], "%Y%m%d")
    end <- format(weather[nrow(weather), "date"], "%Y%m%d")
    ppd.weather <- getPPD(ppd.id, start, end, silo.apiKey) 
    
    # Create full data with NAs for missing days, and merge with weather
    days <- seq(weather[1, "date"], weather[nrow(weather), "date"], by="1 day")
    d <- as.numeric(format(days, "%d"))
    m <- as.numeric(format(days, "%m"))
    y <- as.numeric(format(days, "%Y"))    
    full <- data.frame(date=days, year=y, month=m, day=d)
    w <- merge(weather, full, by=c("date", "year", "month", "day"), all=T)
    
    # Index of missing days
    indx <- which(is.na(w$rain))
    
    # Copy in data for air_temp_min, air_temp_max, air_temp_ave, rain, total_solar, 
    # humidity_max, humidity_min, humidity_ave, evaporation
    
    w[indx, "air_temp_min"] <- ppd.weather[indx, "T.Max"]
    w[indx, "air_temp_max"] <- ppd.weather[indx, "T.Min"]
    w[indx, "air_temp_ave"] <- (ppd.weather[indx, "T.Min"] + ppd.weather[indx, "T.Max"])/2
    w[indx, "rain"]         <- ppd.weather[indx, "Rain"]
    w[indx, "total_solar"]  <- ppd.weather[indx, "Radn"] * 1000
    w[indx, "humidity_max"] <- ppd.weather[indx, "RHmaxT"]
    w[indx, "humidity_min"] <- ppd.weather[indx, "RHminT"]
    w[indx, "humidity_ave"] <- (ppd.weather[indx, "RHminT"] + ppd.weather[indx, "RHmaxT"])/2
    w[indx, "evaporation"]  <- ppd.weather[indx, "FAO56"]
    weather <- w
    }
  
  return(weather)
}

#' Download DPIRD hourly weather data for a period of one year (may contain missing values)
#' 
#' Download DPIRD daily weather data for a period of one year (may contain missing values)
#'
#' @param id Weather station ID.
#' @param start Start date (Date).
#' @param end End date (Date).
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getDPIRDhourly <- function(id, start, end, apiKey) {
  # Get hourly data for the year 
  # Note: "Hour recording date ranges are restricted to two months at a time."
  
  end <- min(end, as.Date(Sys.time()))
  data <- NULL
  
  # Two months after start at a time
  this <- min(seq(from=start, by="month", length.out=3)[3]-1, end)
  while (this <= end) {
    from <- format(start, "%Y-%m-%d")
    to <- format(this, "%Y-%m-%d")
    g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                             "&fromDate=", from, 
                             "&toDate=", to, 
                             "&api_key=", apiKey)))
    #print(g)
    data <- rbind(data, g$result)
    start <- this+1
    this <- seq(from=start, by="month", length.out=3)[3]-1
  }
  
  for (j in c(3:22, 24:30)) data[, j] <- as.numeric(data[,j])
  
  data$year <- as.numeric(substr(data$record_datetime, 1, 4))
  data$month <- as.numeric(substr(data$record_datetime, 6, 7))
  data$day <- as.numeric(substr(data$record_datetime, 9, 10))
  data$hour <- as.numeric(substr(data$record_datetime, 12, 13))
  
  #print(names(data))
  
  data$date <- as.Date(data$record_datetime, "%Y-%m-%d %H:%M:%S")
  data$record_datetime <- as.POSIXct(data$record_datetime)
  
  #cat("wtf\n")
  
  return(data)
}



#' Download DPIRD hourly weather data for a period of one year (may contain missing values)
#' 
#' Download DPIRD daily weather data for a period of one year (may contain missing values)
#'
#' @param id Weather station ID.
#' @param year Year (numeric YYYY).
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getDPIRDhourlyByYear <- function(id, year, apiKey) {
  # Get hourly data for the year 
  # Note: "Hour recording date ranges are restricted to two months at a time."
  
  result <- NULL
  
  from <- paste0(year, "-01-01")
  to <- paste0(year, "-02-28")
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey)))
  result <- rbind(result, g$result)
  
  from <- paste0(year, "-03-01")
  to <- paste0(year, "-04-30")
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  result <- rbind(result, g$result)
  
  from <- paste0(year, "-05-01")
  to <- paste0(year, "-06-30")
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  result <- rbind(result, g$result)
  
  from <- paste0(year, "-07-01")
  to <- paste0(year, "-08-31")
  g <- url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-09-01")
  to <- paste0(year, "-10-31")
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  result <- rbind(result, g$result)
  
  from <- paste0(year, "-11-01")
  to <- paste0(year, "-12-31")
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  result <- rbind(result, g$result)
  
  data <- result
  
  for (j in c(3:22, 24:30)) data[, j] <- as.numeric(data[,j])
  
  data$year <- as.numeric(substr(data$record_datetime, 1, 4))
  data$month <- as.numeric(substr(data$record_datetime, 6, 7))
  data$day <- as.numeric(substr(data$record_datetime, 9, 10))
  data$hour <- as.numeric(substr(data$record_datetime, 12, 13))
  
  data$date <- as.Date(data$record_datetime, "%Y-%m-%d %H:%M:%S")
  data$record_datetime <- as.POSIXct(data$record_datetime)
  
  return(data)
}


#' Download DPIRD hourly weather data for a period of one month (may contain missing values)
#' 
#' Download DPIRD daily weather data for a period of one month (may contain missing values)
#'
#' @param id Weather station ID.
#' @param year Year (numeric YYYY).
#' @param month Month (numeric m).
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getDPIRDhourlyByMonth <- function(id, year, month, apiKey) {
  
  m <- ifelse(month < 10, paste0("0", month), month)
  
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  from <- paste0(year, "-", m, "-01")
  to <- paste0(year, "-", m, "-", days[month])
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  data <- g$result
  
  for (j in c(3:22, 24:30)) data[, j] <- as.numeric(data[,j])
  
  data$year <- as.numeric(substr(data$record_datetime, 1, 4))
  data$month <- as.numeric(substr(data$record_datetime, 6, 7))
  data$day <- as.numeric(substr(data$record_datetime, 9, 10))
  data$hour <- as.numeric(substr(data$record_datetime, 12, 13))
  
  data$date <- as.Date(data$record_datetime, "%Y-%m-%d %H:%M:%S")
  data$record_datetime <- as.POSIXct(data$record_datetime)
  
  return(data)
}

#' Download DPIRD hourly weather data for a period of one day (may contain missing values)
#' 
#' Download DPIRD daily weather data for a period of one day (may contain missing values)
#'
#' @param id Weather station ID.
#' @param year Year (numeric YYYY).
#' @param month Month (numeric m).
#' @param day Day (numeric d).
#' @param apiKey API key (available from https://www.agric.wa.gov.au/web-apis).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getDPIRDhourlyByDay <- function(id, year, month, day, apiKey) {
  
  m <- ifelse(month < 10, paste0("0", month), month)
  d <- ifelse(day < 10, paste0("0", day), day)
  
  from <- to <- paste0(year, "-", m, "-", d)
  g <- fromJSON(url(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                           "&fromDate=", from, 
                           "&toDate=", to, 
                           "&api_key=", apiKey)))
  data <- g$result
  
  for (j in c(3:22, 24:30)) data[, j] <- as.numeric(data[,j])
  
  data$year <- as.numeric(substr(data$record_datetime, 1, 4))
  data$month <- as.numeric(substr(data$record_datetime, 6, 7))
  data$day <- as.numeric(substr(data$record_datetime, 9, 10))
  data$hour <- as.numeric(substr(data$record_datetime, 12, 13))
  
  data$date <- as.Date(data$record_datetime, "%Y-%m-%d %H:%M:%S")
  data$record_datetime <- as.POSIXct(data$record_datetime)
  
  return(data)
}

