

#' Download list of DPIRD SCIENCE weather stations
#' 
#' Download list of DPIRD SCIENCE weather stations - contains Patched Point and DPIRD
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


#' Download 'rainfall to date' data using DPIRD SCIENCE API
#' 
#' Download 'rainfall to date' data using DPIRD SCIENCE API.
#' See https://www.agric.wa.gov.au/climate-weather/rainfall-date for 
#' details on the calculation of cumulative rainfall and rainfall deciles.
#'
#' @param station.id Station id
#' @param forecastDate Forecast date; string "YYYY-mm-dd"
#' @param summerStartDate Summer start date; string "YYYY-mm-dd"
#' @param growingSeasonStartDate Growing season start date; string "YYYY-mm-dd"
#' @param growingSeasonEndDate Growing season end date; string "YYYY-mm-dd"
#' @param apiKey API key (see https://www.agric.wa.gov.au/science-api-20).
#'
#' @author Fiona Evans
#' 
#' @return List of data frames.
#' @export
getRTD <- function(station.id, forecastDate, summerStartDate, growingSeasonStartDate,
                   growingSeasonEndDate, apiKey){
  science <- 'https://api.dpird.wa.gov.au/v2/science/rainfall/'
  
  
  url <- paste0(science, station.id, 
                "?forecastDate=", forecastDate,
                "&summerStartDate=", summerStartDate,
                "&growingSeasonStartDate=", growingSeasonStartDate,
                "&growingSeasonEndDate=", growingSeasonEndDate,
                "&api_key=", apiKey)
  
  res <- fromJSON(url(url))
  
  # Summary
  summary <- res$data$summary
  
  # Rainfall and cumulative rainfall
  rain <- res$data$currentSeasonalRainfall
  rain$date <- as.Date(rain$date, "%Y-%m-%d")
  rain <- subset(rain, date < as.Date(forecastDate, "%Y-%m-%d"))
  
  # Rainfall projections based on climatology
  proj <- proj2df(res$data$projectedSeasonalRainfall)
  
  # Historical rainlfall climatology
  hist <- hist2df(res$data$historicalRainfall, growingSeasonStartDate,
                  growingSeasonEndDate)
  hist <- subset(hist, date < as.Date(forecastDate, "%Y-%m-%d"))
  
  tmp <- merge(rain, hist, by="date", all=T)
  data <- merge(tmp, proj, by="date", all=T)
  
  list(summary=summary, data=data)
}

proj2df <- function(tmp) {
  dec <- matrix(ncol=9, nrow=length(tmp$deciles)) 
  for (i in 1:nrow(dec)) {
    dec[i, ] <- tmp$deciles[[i]][, 2]
  }
  dec <- as.data.frame(dec)
  names(dec) <- paste0("proj", c(1:9))
  dec$date <- as.Date(tmp$date, "%Y-%m-%d")
  dec
}

hist2df <- function(tmp, growingSeasonStartDate,
                    growingSeasonEndDate) {
  dec <- matrix(ncol=9, nrow=length(tmp$deciles)) 
  for (i in 1:nrow(dec)) {
    dec[i, ] <- tmp$deciles[[i]][, 2]
  }
  dec <- as.data.frame(dec)
  names(dec) <- paste0("decile", c(1:9))
  dec$date <- seq(from=as.Date(growingSeasonStartDate, "%Y-%m-%d"),
                  to=as.Date(growingSeasonEndDate, "%Y-%m-%d"), by = "1 day")
  #dec$date <- as.Date(paste0(year, "-", tmp$date), "%Y-%m-%d")
  dec
}


#' Download  'soil water' data using DPIRD SCIENCE API
#' 
#' Download 'soil water' data using DPIRD SCIENCE API.
#' See https://www.agric.wa.gov.au/climate-weather/soil-water-tool
#' for details on the soil water model used.
#'
#' @param station.id Station id
#' @param startDate Summer start date (sw is initialised at zero); string "YYYY-mm-dd"
#' @param endDate End date; string "YYYY-mm-dd"
#' @param soilType Soil type; string element of c("gravel", "shallow-soil", "sand", "sandy-earth", "shallow-sandy-duplex", "deep-sand-duplex", "shallow-loamy-duplex", "deep-loamy-duplex", "loamy-earth", "clay")
#' @param faoInitialisationDays number of days in crop initialisation, used in calculation of crop evapotranspiration
#' @param faoDevelopmentDays number of days in crop development period, used in calculation of crop evapotranspiration 
#' @param faoMidSeasonDays number of days in crop mid-season period, used in calculation of crop evapotranspiration
#' @param faoLateSeasonDays number of days in crop late-season period, used in calculation of crop evapotranspiration
#' @param faoInitialisationCropCoefficient proportion of potential evaporation used by crop during initialisation period
#' @param faoDevelopmentCropCoefficient proportion of potential evaporation used by crop during development period
#' @param faoMidSeasonCropCoefficient proportion of potential evaporation used by crop during mid-season period
#' @param faoLateSeasonCropCoefficient proportion of potential evaporation used by crop during late-season period
#' @param faoBreakOfSeason3Days25April mm of rainfall after April 25th that triggers the break of season
#' @param faoBreakOfSeason3Days5June mm of rainfall after June 5th that triggers the break of season
#' @param apiKey API key (see https://www.agric.wa.gov.au/science-api-20).
#' 
#' @author Fiona Evans
#' 
#' @return List of data frames.
#' @export
getSoilWater <- function(station.id, startDate, endDate,
                   soilType = "shallow-sandy-duplex",
                   faoInitialisationDays = 15,
                   faoDevelopmentDays = 20, 
                   faoMidSeasonDays = 80,
                   faoLateSeasonDays = 45,
                   faoInitialisationCropCoefficient = 0.05,
                   faoDevelopmentCropCoefficient = 0.1,
                   faoMidSeasonCropCoefficient = 0.45,
                   faoLateSeasonCropCoefficient = 0.2,
                   faoBreakOfSeason3Days25April = 15,
                   faoBreakOfSeason3Days5June = 5,
                   apiKey){
  
  science <- 'https://api.dpird.wa.gov.au/v2/science/soilwater'
  
  
  url <- paste0(science, "?stationCode=", station.id, 
                "&startDate=", startDate,
                "&endDate=", endDate,
                "&soilType=", soilType,
                "&faoInitialisationDays=", faoInitialisationDays,
                "&faoDevelopmentDays=", faoDevelopmentDays, 
                "&faoMidSeasonDays=", faoMidSeasonDays,
                "&faoLateSeasonDays=", faoLateSeasonDays,
                "&faoDevelopmentCropCoefficient=", faoDevelopmentCropCoefficient,
                "&faoDevelopmentCropCoefficient=", faoDevelopmentCropCoefficient,
                "&faoMidSeasonCropCoefficient=", faoMidSeasonCropCoefficient,
                "&faoLateSeasonCropCoefficient=", faoLateSeasonCropCoefficient,
                "&faoBreakOfSeason3Days25April=", faoBreakOfSeason3Days25April,
                "&aoBreakOfSeason3Days5June=", faoBreakOfSeason3Days5June,
                "&api_key=", apiKey)
  
  res <- fromJSON(url(url))
  
  
  data <- res$data$soilWater
  data$date <- as.Date(data$date, "%Y-%m-%d")
  
  brk <- as.Date(res$data$breakOfSeason, "%Y-%m-%d")
  
  list(data=data, breakOfSeason=brk)
}
