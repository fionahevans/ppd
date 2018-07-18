
#' Convert downloaded PPD data to APSIM .met file
#' 
#' Convert downloaded PPD data to APSIM .met file
#'
#' @param id Weather station ID.
#' @param weather data frame containing PPD weather data downloaded using getPPD().
#' @param fileName name of .met file to write output to.
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
ppd2apsimMET <- function(id, weather, fileName="weather.met") {
  
  i <- which(STATIONS$SITE_NO == id)
  stationName <- tolower(STATIONS[i, "STATION_NAME"])
  stationName <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                       stationName, perl=TRUE)
  
  sink(file=fileName)
  
  weather$DATE <- as.Date(weather$Date, "YYYY/mm/dd")
  weather$Year <- as.numeric(format(weather$DATE, "%Y"))
  weather$Month <- as.numeric(format(weather$DATE, "%m"))
  years <- range(weather$Year)
  
  cat(paste0('!title = ', stationName, ' ', years[1], '-', years[2], '\n'))
  cat('\n')
  cat('[weather.met.weather]\n')
  cat(paste0('!station number = ', STATIONS[i, "SITE_NO"], '\n'))
  cat(paste0('!station name = ', stationName, '\n'))
  cat(paste0('latitude = ', STATIONS[i, "LATITUDE"], '\n'))
  cat(paste0('longitude = ', STATIONS[i, "LONGITUDE"], '\n'))
  
  # From https://www.apsim.info/Portals/0/OtherProducts/tav_amp.pdf:
  # Amp is obtained by averaging the mean daily temperature of each month over the 
  # entire data period resulting in twelve mean temperatures, and then subtracting the 
  # minimum of these values from the maximum. Tav is obtained by averaging the twelve 
  # mean monthly temperatures.
  weather$T.Avg <- (weather$T.Max+ weather$T.Min)/2
  
  monthly <- aggregate(T.Avg ~ Month, weather , mean )
  
  cat(paste0(' tav =  ', round(mean(monthly$T.Avg), 2),  ' (oC)     ! annual average ambient temperature\n'))
  cat(paste0(' amp =  ', round(max(monthly$T.Avg) - min(monthly$T.Avg), 2), ' (oC)     ! annual amplitude in mean monthly temperature\n'))
  
  tav <- mean(monthly$T.A)
 
  cat('\n')
  sink()
  
  df <- subset(weather, select=c("Year", "Day", "Radn", "T.Max", "T.Min", "Rain", "Evap"))
  
  cnames <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap')
  nms <- c('()', '()', '(MJ/m2)', '(oC)', '(oC)', '(mm)', '(mm)')
  n      <- as.matrix(nchar(nms))
  
  d <- apply(df, 2, format)
  n <- apply(cbind(n, nchar(d[1,])), 1, max)

  
  fmts <- paste0("%", n, "s")
  for(i in 1:length(cnames)) {
    cnames[i] <- sprintf(fmts[i], cnames[i])
    nms[i] <- sprintf(fmts[i], nms[i])
    d[,i] <- sprintf(fmts[i], trimws(d[,i]))
  }
  d <- rbind(cnames, nms, d)
  
  write.table(d, file=fileName, append=T, sep="  ", quote=F, row.names=F, col.names=F)
  
  
}


#' Convert downloaded DPIRD data to APSIM .met file
#' 
#' Convert downloaded DPIRD data to APSIM .met file
#'
#' @param id Weather station ID.
#' @param weather data frame containing DPIRD weather data downloaded using getDPIRDdaily().
#' @param stations list of DPIRD weather stations downloaded using getDPIRDstations().
#' @param fileName name of .met file to write output to.
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
dpird2apsimMET <- function(id, weather, stations, fileName="weather.met") {
  
  i <- which(stations$station_id == id)
  stationName <- paste(tolower(stations[i, "name"]), "(DPIRD)")
  stationName <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", 
                      stationName, perl=TRUE)
  
  sink(file=fileName)
  
  weather$DATE <- weather$date
  weather$Year <- weather$year
  weather$Day <- as.numeric(format(weather$DATE, "%j"))
  weather$Radn <- weather$total_solar/1000
  weather$T.Max <- weather$air_temp_max
  weather$T.Min <- weather$air_temp_min
  weather$Rain <- weather$rain
  weather$Evap <- weather$evaporation
  
  
  # Need to fill missing data
  
  years <- range(weather$Year)
  
  cat(paste0('!title = ', stationName, ' ', years[1], '-', years[2], '\n'))
  cat('\n')
  cat('[weather.met.weather]\n')
  cat(paste0('!station number = ', id, '\n'))
  cat(paste0('!station name = ', stationName, '\n'))
  cat(paste0('latitude = ', STATIONS[i, "LATITUDE"], '\n'))
  cat(paste0('longitude = ', STATIONS[i, "LONGITUDE"], '\n'))
  
  # From https://www.apsim.info/Portals/0/OtherProducts/tav_amp.pdf:
  # Amp is obtained by averaging the mean daily temperature of each month over the 
  # entire data period resulting in twelve mean temperatures, and then subtracting the 
  # minimum of these values from the maximum. Tav is obtained by averaging the twelve 
  # mean monthly temperatures.
  weather$T.Avg <- (weather$T.Max+ weather$T.Min)/2
  
  monthly <- aggregate(T.Avg ~ Month, weather , mean )
  
  cat(paste0(' tav =  ', round(mean(monthly$T.Avg), 2),  ' (oC)     ! annual average ambient temperature\n'))
  cat(paste0(' amp =  ', round(max(monthly$T.Avg) - min(monthly$T.Avg), 2), ' (oC)     ! annual amplitude in mean monthly temperature\n'))
  
  tav <- mean(monthly$T.A)
  
  cat('\n')
  sink()
  
  df <- subset(weather, select=c("Year", "Day", "Radn", "T.Max", "T.Min", "Rain", "Evap"))
  
  cnames <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap')
  nms <- c('()', '()', '(MJ/m2)', '(oC)', '(oC)', '(mm)', '(mm)')
  n      <- as.matrix(nchar(nms))
  
  d <- apply(df, 2, format)
  n <- apply(cbind(n, nchar(d[1,])), 1, max)
  
  
  fmts <- paste0("%", n, "s")
  for(i in 1:length(cnames)) {
    cnames[i] <- sprintf(fmts[i], cnames[i])
    nms[i] <- sprintf(fmts[i], nms[i])
    d[,i] <- sprintf(fmts[i], trimws(d[,i]))
  }
  d <- rbind(cnames, nms, d)
  
  write.table(d, file=fileName, append=T, sep="  ", quote=F, row.names=F, col.names=F)
  
  
}
