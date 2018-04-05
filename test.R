# roxygen2::roxygenize('.')


library(ppd)
httr::set_config( config( ssl_verifypeer = 0L ))


# SILO data
i <- which(STATIONS$STATION_NAME == "MERREDIN")
weather <- getPPD(STATIONS$SITE_NO[i], "19000101", "20171231", username="MUEVANS", password="FIONA6151")


# Download all SILO data and save to disk
for (i in 1:nrow(STATIONS)){
  weather <- getPPD(STATIONS$SITE_NO[i], "19750101", "20161231", username="MUEVANS", password="FIONA6151")
  write.csv(weather, file=paste0(STATIONS$SITE_NO[i], ".csv"))
}



# DPIRD data

apiKey <- "B7341B779DDAB41C87AFF7A4.apikey"

stations <- getDPIRDstations(apiKey)

data <- getDPIRDdaily("MI", 2017, apiKey)

data <- getDPIRDhourlyByYear("MI", 2017, apiKey)

