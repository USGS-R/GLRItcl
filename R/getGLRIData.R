#' Get QW data for multiple sites
#'
#' Gets the basic data from the water quality portal.
#'
#' @param siteNumber string vector of just USGS site ID numbers 
#' @param startDate string date in format yyyy-mm-dd
#' @param OWC logical if using OWC, parameter codes taken from pcodeINFO dataset
#' @param pCodes character vector if not using OWC, pCodes list
#' @keywords data import from web service
#' @return dataframe 
#' @export
#' @examples
#' OWCdata <- getGLRIData(c('04010500','04024000'), '2013-01-01',OWC=TRUE)
#' nonOWCdata <- getGLRIData(c('04010500','04024000'), '2013-01-01',OWC=FALSE, c('00010','00300'))
getGLRIData <- function(siteNumber,startDate,OWC=TRUE,pCodes=NULL){
  setInternet2(use=NA)
  setInternet2(use=FALSE)
  setInternet2(use=NA)
  
  siteNumber <- paste("USGS-",siteNumber,sep="")  
  siteNumber <- paste(siteNumber, collapse=";")
  
  startDate <- format(as.Date(startDate), format="%m-%d-%Y")
  
  if (OWC){
    pCodes <- pcodeINFO$parameter_cd[!is.na(pcodeINFO$class)]
  }
  
  pCodes <- paste(pCodes, collapse=";")
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?siteid="
  url <- paste(baseURL,
               siteNumber,
               "&startDateLo=",
               startDate,
               "&pCode=",
               pCodes,
               "&countrycode=US&mimeType=tsv",sep = "")
  
  suppressWarnings(retval <- read.delim(url, header = TRUE, quote="\"", dec=".", sep='\t', colClasses=c('character'), fill = TRUE))
  return(retval)
  
}