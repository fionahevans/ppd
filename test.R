# roxygen2::roxygenize('.')


library(ppd)
httr::set_config( config( ssl_verifypeer = 0L ))

for (i in 1:nrow(STATIONS)){
  weather <- getPPD(STATIONS$SITE_NO[i], "19750101", "20161231", username="MUEVANS", password="FIONA6151")
  write.csv(weather, file=paste0(STATIONS$SITE_NO[i], ".csv"))
}




i <- which(STATIONS$STATION_NAME == "MERREDIN")
weather <- getPPD(STATIONS$SITE_NO[i], "19750101", "20171231", username="MUEVANS", password="FIONA6151")
