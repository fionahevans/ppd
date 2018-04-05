

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
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations.json?api_key=", apiKey))
  
  stations <- fromJSON(content(g, "text"))$result
  
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
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/dailysummary.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- fromJSON(content(g, "text"))$result
  
  for (j in c(3,5,7,8,10,12,13,14,16,18,19,20,21,22,23,25,27,28,30,
              33,35,37,38,40,42,43)) result[, j] <- as.numeric(result[,j])
  
  result$year <- as.numeric(substr(result$record_date, 1, 4))
  result$month <- as.numeric(substr(result$record_date, 6, 7))
  result$day <- as.numeric(substr(result$record_date, 9, 10))
  
  result$date <- as.Date(result$record_date, "%Y-%m-%d")
  
  return(result)
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
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-03-01")
  to <- paste0(year, "-04-30")
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-05-01")
  to <- paste0(year, "-06-30")
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-07-01")
  to <- paste0(year, "-08-31")
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-09-01")
  to <- paste0(year, "-10-31")
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  from <- paste0(year, "-11-01")
  to <- paste0(year, "-12-31")
  g <- GET(paste0("https://api.agric.wa.gov.au/v1/weatherstations/hourrecordings.json?stationId=", id, 
                  "&fromDate=", from, 
                  "&toDate=", to, 
                  "&api_key=", apiKey))
  result <- rbind(result, fromJSON(content(g, "text"))$result)
  
  data <- result
  
  for (j in c(3:22, 24:30)) result[, j] <- as.numeric(result[,j])
  
  data$year <- as.numeric(substr(data$record_datetime, 1, 4))
  data$month <- as.numeric(substr(data$record_datetime, 6, 7))
  data$day <- as.numeric(substr(data$record_datetime, 9, 10))
  data$hour <- as.numeric(substr(data$record_datetime, 12, 13))
  
  data$date <- as.Date(data$record_datetime, "%Y-%m-%d %H:%M:%S")
  
  return(data)
}
