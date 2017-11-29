# roxygen2::roxygenize('.')

library(devtools)
load_all()

library(RODBC)

# Open connection to Oracle databases
con <- odbcConnect("BSIPEDSDEV", "SCIENCE", "scienced")
metcon <- odbcConnect("METUSER", "METUSER", "METUSER")

soil.types <- sqlFetch(con, "CLI_SW_SOILS")

site.no <- 10115 # Quellington

# Fetch daily data for the station
climate.data <- sqlQuery(metcon, 
                         paste("select SITE_NO, YEAR, MONTH, DAY, MAX_TEMP, MIN_TEMP, RAIN, EVAPORATION, RADIATION",
                               " from MET.CLIMATE_PPD", " where (SITE_NO = '", site.no, "')", sep=""))  

# Add date
d <- ifelse(climate.data$DAY < 10, paste("0", climate.data$DAY, sep=''), climate.data$DAY)
m <- ifelse(climate.data$MONTH < 10, paste("0", climate.data$MONTH, sep=''), climate.data$MONTH)
climate.data$DATE <- as.Date(paste(d, m, climate.data$YEAR, sep=''), "%d%m%Y") 

head(climate.data)

climate.data <- subset(climate.data, DATE >= as.Date("01112015", "%d%m%Y") & DATE <= as.Date("31102016", "%d%m%Y"))

save.image()

# Run model
tmp <- soil.water(climate.data, soil.types[6,])

par(mai=c(0.4, 0.4, 0.1, 0.1))
plot(climate.data$DATE, tmp, type="l", xlab="Date", ylab="Soil water", col="brown", lwd=2)
for (i in 1:nrow(climate.data)) {
  this <- data.frame(DATE = rep(climate.data$DATE[i], 2), RAIN=c(0, climate.data$RAIN[i]))
  with(this, lines(DATE, RAIN, col="navy", lwd=2))
}
lines(climate.data$DATE, tmp, col="brown", lwd=2)


######## Now to add in the crop ET model


# Grains (small), Mediterranean
wheat.et.table <- data.frame(stage=c("init-dev", "mid", "late"),
                              length=c(50, 60, 40),
                              coef=c(0.3, 1.15, 0.25))

# WA version
wheat.et.table <- data.frame(stage=c("init-dev", "mid", "late"),
                              length=c(50, 40, 45),
                              coef=c(0.3, 1.15, 0.25))

# Get break of season
year <- as.numeric(format(climate.data[nrow(climate.data), "DATE"], "%Y"))
b <- break.of.season(climate.data, year, mm1=15)

# Run model
tmp <- soil.water(climate.data, soil.types[6,], 
                  season.break=which(climate.data$DATE == b), wheat.et.table)

par(mai=c(0.4, 0.4, 0.1, 0.1))
plot(climate.data$DATE, tmp, type="l", xlab="Date", ylab="Soil water", col="brown", lwd=2)
for (i in 1:nrow(climate.data)) {
  this <- data.frame(DATE = rep(climate.data$DATE[i], 2), RAIN=c(0, climate.data$RAIN[i]))
  with(this, lines(DATE, RAIN, col="navy", lwd=2))
}
lines(climate.data$DATE, tmp, col="brown", lwd=2)

abline(v=b)
abline(v=(b+wheat.et.table[1, "length"]))
abline(v=(b+sum(wheat.et.table[1:2, "length"])))



