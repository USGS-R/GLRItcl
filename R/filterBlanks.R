#' Filter the blanks and quality control sample data 
#'
#' Filter the blanks and quality control sample data.
#'
#' @param rawData data.frame from a water quality portal pull
#' @keywords filter
#' @return rawData_useful dataframe 
#' @export
#' @examples
#' QWPortalGLRI <- QWPortalGLRI
#' noBlanksData <- filterBlanks(QWPortalGLRI)
filterBlanks <- function(rawData){
  
  rawData$index <- 1:nrow(rawData)
  
#   colsToRemove <- c("Sample-Composite Without Parents" , "Quality Control Sample-Field Blank", "Quality Control Sample-Spike Solution")
  colsToRemove <- c("Quality Control Sample-Field Blank", "Quality Control Sample-Spike Solution")
  
  rawDataNoBlanks <- rawData[!(rawData[,4] %in% colsToRemove),]
  rawData_Blanks <- rawData[(rawData[,4] %in% colsToRemove),]
  
  # Some '-Other's are replicates
  subPull <- rawDataNoBlanks[grep( "Quality Control Sample-Other",rawDataNoBlanks[,4]),]
  subSites <- unique(subPull$MonitoringLocationIdentifier)
  indexToRemove <- vector()
  
  for (i in subSites){
    subSubPull <- subPull[i == subPull$MonitoringLocationIdentifier,]
    subPCodes <- unique(subSubPull$USGSPCode)
    for (j in subPCodes){
      subSubSub <- subSubPull[j == subSubPull$USGSPCode,]
      subSubSub$dates <- as.POSIXct(paste(subSubSub$ActivityStartDate,subSubSub$ActivityStartTime.Time,sep=" "),tz="UTC")
      indexToRemove <- append(indexToRemove, subSubSub$index[subSubSub$dates == min(subSubSub$dates)])
    }
  }
  
  rawData_useful <- rawDataNoBlanks[-indexToRemove,]
  
  return(rawData_useful)
  
}