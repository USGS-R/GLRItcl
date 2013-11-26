#' Calculate and summarize stats on GLRI data
#'
#' Filter the raw data into a simple dataframe.
#'
#' @param dataByClass data.frame 
#' @keywords stat summary
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' filteredData <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
#' wideDF <- wideGLRIData(filteredData)
#' keyDF <- pcodeINFO
#' classCol <- "class"
#' pCodeCol <- "parameter_cd"
#' dataByClass <- PCodeClassSummary(wideDF,keyDF,pCodeCol,classCol)
#' statClassSummary(dataByClass)
statClassSummary <- function(dataByClass){
  
  sites <- unique(dataByClass$site)
  
  classes <- unique(pcodeINFO$class)
  classes <- classes[!is.na(classes)]
  
  colNames <- colnames(dataByClass)
  colNamesSplitPreffix <- sapply(strsplit(colNames, "_"),function(x)x[1])
  colNamesSplitSuffix <- sapply(strsplit(colNames, "_"),function(x)x[2])
  
  sites <- unique(stationINFO$station.nm)
  names(sites) <- stationINFO$fullSiteID[which(stationINFO$station.nm %in% sites)]
  
  for (i in 1:length(classes)){
    indexClass <- which(colNamesSplitSuffix == classes[i])
    indexValue <- which(colNamesSplitPreffix == "sumOfValues")
    indexPercent <- which(colNamesSplitPreffix == "percentDetected")
    indexValue <- indexClass[which(indexClass %in% indexValue)]
    indexPercent <- indexClass[which(indexClass %in% indexPercent)]
    
    #Values:
    subDF <- dataByClass[,c(1:4,indexValue, indexPercent)]
    
    DF <- subset(aggregate(subDF,by=list(subDF$site),FUN=length),select=Group.1:site)
    colnames(DF) <- c("site","count")
    DF$startDate <- aggregate(subDF$ActivityStartDateGiven,by=list(subDF$site),FUN=min, na.rm=TRUE)$x
    DF$endDate <- aggregate(subDF$ActivityStartDateGiven,by=list(subDF$site),FUN=max, na.rm=TRUE)$x
    DF$mean <- aggregate(subDF[[5]],by=list(subDF$site),FUN=mean, na.rm=TRUE)$x
    DF$max <- aggregate(subDF[[5]],by=list(subDF$site),FUN=max, na.rm=TRUE)$x
    DF$min <- aggregate(subDF[[5]],by=list(subDF$site),FUN=min, na.rm=TRUE)$x
    DF$median<- aggregate(subDF[[5]],by=list(subDF$site),FUN=median, na.rm=TRUE)$x
    DF$percentDetect <- aggregate(subDF[[6]],by=list(subDF$site),FUN=mean, na.rm=TRUE)$x
    DF$station.nm <- sites[DF$site]
    DF$class <- classes[i]
    
    if (1 == i){
      fullDF <- DF
    } else {
      fullDF <- rbind(fullDF, DF)
    }

  }
  
  return(fullDF)
}