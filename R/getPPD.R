


#' Download PPD data
#' 
#' Download PPD data for a station and return as a data frame
#'
#' @param id Weather station ID.
#' @param start Start date (String YYYYmmdd).
#' @param end End date (String YYYYmmdd).
#' @param silo.apiKey SILO API key (available from https://silo.longpaddock.qld.gov.au/).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getPPD <- function(id, start, end, silo.apiKey) {
  
  url <- paste0("https://siloapi.longpaddock.qld.gov.au/pointdata?apikey=", silo.apiKey)
        
  
  # Get 2016 growing season rainfall 
  httr::set_config( config( ssl_verifypeer = 0L ))
  g <- GET(paste0(url, "&start=", start,  "&finish=", end,  "&station=", id,  "&format=alldata"))
  
  if (g$status_code != 200 ) ret <- NULL
  
  if (g$status_code == 200) {
  
    all.lines <- content(g, "text")
    # split into lines
    lines <- unlist(strsplit(all.lines, "\n"))
    # ditch the header
    lines <- lines[-c(1:49)]
    
    if(lines[1] ==  '\" \"') lines <- lines[-1]
    
    h1 <- unlist(strsplit(lines[1], "\\s+"))
    
    mat <- matrix(unlist(strsplit(lines[3:length(lines)], "\\s+")), byrow=T, ncol=length(h1))
    ret <- data.frame(mat)
    names(ret) <- h1
    ret$Date <- as.Date(ret$Date, "%Y%m%d")
    ret$Date2 <- NULL
    for (i in 2:ncol(ret)) ret[,i] <- as.numeric(as.character(ret[,i]))
  }
  ret
}




#' Download PPD data (Legacy code, replace with getPPD)
#' 
#' Download PPD data for a station and return as a data frame 
#'
#' @param id Weather station ID.
#' @param start Start date (String YYYYmmdd).
#' @param end End date (String YYYYmmdd).
#' @param username SILO username (available from https://legacy.longpaddock.qld.gov.au/silo/).
#' @param password SILO password (available form https://legacy.longpaddock.qld.gov.au/silo/).
#'
#' @author Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export
getPPD.legacy <- function(id, start, end, username, password) {
  
  url <- paste0("https://legacy.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php",
                "?username=", username, "&password=", password)
  
  
  # Get 2016 growing season rainfall 
  httr::set_config( config( ssl_verifypeer = 0L ))
  g <- GET(paste0(url, "&format=alldata&station=", id, 
                  "&start=", start,
                  "&finish=",  end))
  
  all.lines <- content(g, "text")
  # split into lines
  lines <- unlist(strsplit(all.lines, "\n"))
  # ditch the header
  lines <- lines[-c(1:49)]
  
  if(lines[1] ==  "\" \"") lines <- lines[-1]
  
  h1 <- unlist(strsplit(lines[1], "\\s+"))
  
  mat <- matrix(unlist(strsplit(lines[3:length(lines)], "\\s+")), byrow=T, ncol=length(h1))
  ret <- data.frame(mat)
  names(ret) <- h1
  ret$Date <- as.Date(ret$Date, "%Y%m%d")
  ret$Date2 <- NULL
  for (i in 2:ncol(ret)) ret[,i] <- as.numeric(as.character(ret[,i]))
  ret
}
