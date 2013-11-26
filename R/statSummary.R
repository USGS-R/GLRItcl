#' Calculate and summarize stats on GLRI data
#'
#' Filter the raw data into a simple dataframe.
#'
#' @param filteredData data.frame 
#' @keywords stat summary
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' filteredData <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
#' statSummary(filteredData)
statSummary <- function(filteredData){
  sites <- unique(filteredData$site)
  
  DF <- subset(aggregate(filteredData,by=list(filteredData$site,filteredData$USGSPCode),FUN=length),select=Group.1:USGSPCode)
  colnames(DF) <- c("site","PCode", "count")  
  DF$startDate <- aggregate(filteredData$ActivityStartDateUTC,by=list(filteredData$site,filteredData$USGSPCode),FUN=min)$x
  DF$endDate <- aggregate(filteredData$ActivityStartDateUTC,by=list(filteredData$site,filteredData$USGSPCode),FUN=max)$x
  DF$mean <- signif(aggregate(filteredData$valueToUse,by=list(filteredData$site,filteredData$USGSPCode),FUN=mean)$x,3)
  DF$max <- aggregate(filteredData$valueToUse,by=list(filteredData$site,filteredData$USGSPCode),FUN=max)$x 
  DF$min <- aggregate(filteredData$valueToUse,by=list(filteredData$site,filteredData$USGSPCode),FUN=min)$x 
  DF$median <- aggregate(filteredData$valueToUse,by=list(filteredData$site,filteredData$USGSPCode),FUN=median)$x 
  
  if (sum("<" == filteredData$qualifier) > 0){
    cen <- filteredData[("<" == filteredData$qualifier) | (filteredData$valueToUse == 0),]
    cen <- aggregate(cen$qualifier,by=list(cen$site,cen$USGSPCode),FUN=length) 
    colnames(cen)<- c("site", "PCode","numCen")  
    DF <- merge(DF,cen,by=c("site","PCode"), all=TRUE)
    DF$numCen <- ifelse(is.na(DF$numCen),0,DF$numCen)
    DF$percent <- signif(100-100*DF$numCen/DF$count,3)
  } else {
    DF$numCen <- 0
    DF$percent <- 100
  }

  DF$site.no <- sapply(strsplit(DF$site, "-"),function(x)x[2])
  
  sites <- unique(stationINFO$station.nm)
  names(sites) <- stationINFO$site.no[which(stationINFO$station.nm %in% sites)]
  DF$station.nm <- sites[DF$site.no]
  
  pcodes <- unique(pcodeINFO$parameter_nm)
  names(pcodes) <- pcodeINFO$parameter_cd[which(pcodeINFO$parameter_nm %in% pcodes)]
  DF$parameter_nm <- pcodes[DF$PCode]
  
  pClass <- names(pcodes)
  names(pClass) <- pcodeINFO$class[which(pcodeINFO$parameter_cd %in% pClass)]
  DF$class <- names(pClass[DF$PCode])
  
  return(DF)
}