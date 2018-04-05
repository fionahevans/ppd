ppd:  R package containing functions for accessing SILO Patched Point weather data using their URL Web Service.
====================================================

See https://legacy.longpaddock.qld.gov.au/silo/how_to_obtain_data.html for information on how to obtain a username and password for the web service.

Includes a data frame STATIONS containing weather stations with good long term records in south west Western Australia.

Functions:
* nearestStations: Find the nearest stations (in STATIONS) to a given longitude and latitude
* getPPD: Download PPD data fromSILO and store as a data frame
* apsimMET: Convert downloaded PPD data to APSIM .met file

Planned future functionality:
* function getClimatology is in development - do not use
* functions for downloading daily DPIRD rain radar data
*	ability to download filled daily DPIRD weather station data via API (APIs not yet released)
*	ability to download DPIRD soil data via API and insert into .apsim files (APIs not yet released)

Note: DPIRD = Department of Primary Industries and Regional Development, Western Australia. See https://www.agric.wa.gov.au/web-apis 

