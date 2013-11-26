#' Filter the raw data into long dataframe.
#'
#' Filter the raw data into a simple dataframe. Output dataframe is in long format.
#'
#' @param rawData data.frame
#' @param valueToUseFunction function that has input qualifier, value, detection limit, and outputs valueToUse
#' @keywords filter
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' filteredOWC <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
filterGLRIData <- function(rawData, valueToUseFunction){

  qualifier <- ifelse((
      (
        rawData$ResultDetectionConditionText == "Not Detected" # This gives result_cd = "<" or "U"
        |
        rawData$ResultDetectionConditionText == "Detected Not Quantified"  # This gives result_cd = "M" or "N"
      )
    | 
      (
        rawData$ResultMeasureValue < rawData$DetectionQuantitationLimitMeasure.MeasureValue #not sure if this ever happens...it does!
      )
    ), "<", "")

  
  trimmedDF <- data.frame(USGSPCode = rawData$USGSPCode,row.names = NULL)  
  trimmedDF$site <- rawData$MonitoringLocationIdentifier
  
#   tz <- c(5,5,4,6) #This is for usual USGS
  tz <- c(5,6,5,6)  #This is for 8 virus sites 
  names(tz) <- c("EST","CDT","EDT","CST")
  timeZone <- rawData$ActivityStartTime.TimeZoneCode
  
  trimmedDF$tz <- timeZone
  trimmedDF$ActivityStartDateGiven <- as.POSIXct(paste(rawData$ActivityStartDate,rawData$ActivityStartTime.Time,sep=" "),tz="UTC")
  trimmedDF$ActivityStartDateUTC <- trimmedDF$ActivityStartDateGiven - tz[timeZone]*3600
  trimmedDF$ActivityStartDateCurrentLocal <- format(trimmedDF$ActivityStartDateUTC, tz="")

  x <- strsplit(as.character(rawData$ActivityEndTime.Time),":")
  xHours <- sapply(x, function(x) x[1])
  xMin <- sapply(x, function(x) x[2])
  x2 <- paste(xHours,xMin,"00",sep=":")
  rawData$ActivityEndTime.Time <- ifelse("NA:NA:00"==x2, "", x2)
  
  trimmedDF$ActivityEndDateGiven <- as.POSIXct(strptime(paste(rawData$ActivityEndDate,rawData$ActivityEndTime.Time,sep=" "), "%Y-%m-%d %H:%M:%S"),tz="UTC")
  trimmedDF$ActivityEndDateUTC <- trimmedDF$ActivityEndDateGiven - tz[timeZone]*3600
  trimmedDF$ActivityEndDateCurrentLocal <- format(trimmedDF$ActivityEndDateUTC, tz="")  
  
  trimmedDF$qualifier <- qualifier
  
  trimmedDF$value <- suppressWarnings(as.numeric(rawData$ResultMeasureValue))
  trimmedDF$detectionLimit <- suppressWarnings(as.numeric(rawData$DetectionQuantitationLimitMeasure.MeasureValue))
  trimmedDF$HydrologicCondition <- rawData$HydrologicCondition
  trimmedDF$HydrologicEvent <- rawData$HydrologicEvent
  trimmedDF$valueToUse <- valueToUseFunction(qualifier,trimmedDF$value,trimmedDF$detectionLimit)

  row.names(trimmedDF) <- NULL
  return(trimmedDF)
}