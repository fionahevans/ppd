# Add Stations Australia wide

require(data.table)
require(tidyverse)




load("data/STATIONS.RData")

url <- "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=15540&radius=100000"
onlineStations <- fread(url)
colnames(onlineStations) <- c("SITE_NO","STATION_NAME","LATITUDE","LONGITUDE","STATE","HEIGHT","TYPE","UNKNOWN")
onlineStations <- onlineStations[,-8]


STATIONS<-full_join(STATIONS,onlineStations, by =c("SITE_NO","STATION_NAME","LATITUDE","LONGITUDE","STATE","HEIGHT"))


save.image("data/STATIONS.RData")