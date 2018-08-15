ppd:  R package containing functions for accessing SILO Patched Point weather data using their URL Web Service.
====================================================

See https://silo.longpaddock.qld.gov.au/ for information on how to obtain an API key for accessing SILO data.

Includes a data frame STATIONS containing weather stations with good long term records in south west Western Australia.

Functions:
* nearestStations: Find the nearest stations (in STATIONS) to a given longitude and latitude
* getPPD: Download PPD data from SILO and store as a data frame
* ppd2apsimMET: Convert downloaded PPD data to APSIM .met file
* getDPIRDstations: Download list of DPIRD weather stations
* getDPIRDdaily: Download DPIRD daily weather data for one year (may contain missing values)
* getDPIRDhourlyByYear: Download DPIRD daily weather data for a period of one year (may contain missing values)

Planned future functionality:
* function getClimatology is in development - do not use


Note: DPIRD = Department of Primary Industries and Regional Development, Western Australia. See https://www.agric.wa.gov.au/web-apis 

Note: If you want to obtain current weather observation data or forecasts from BoM, see https://github.com/ropensci/bomrang.

