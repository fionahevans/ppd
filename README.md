ppd:  R package containing functions for accessing SILO Patched Point weather data using their URL Web Service.
====================================================

See https://www.longpaddock.qld.gov.au/silo/how_to_obtain_data.html for information on how to obtain a username and password for the web service.

Includes a data frame STATIONS containing weather stations with good long term records in south west Western Australia.

Functions:
* nearestStations: Find the nearest stations (in STATIONS) to a given longitude and latitude
* getPPD: Download PPD data fromSILO and store as a data frame
* apsimMET: Convert downloaded PPD data to APSIM .met file

Function getClimatology is in development - do not use.