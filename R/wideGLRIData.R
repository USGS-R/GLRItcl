#' Convert long GLRI dataframe to wide
#'
#' Filter the filtered data into a wide format.
#'
#' @param filteredData data.frame 
#' @keywords filter
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' filteredData <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
#' wideGLRIData(filteredData)
wideGLRIData <- function(filteredData){
  colNames <- colnames(filteredData)
  index <- which(colNames != "tz" & colNames != "ActivityStartDateCurrentLocal" & colNames != "ActivityEndDateCurrentLocal" & colNames != "ActivityStartDateUTC" & colNames != "ActivityEndDateUTC" & colNames != "ActivityEndDateGiven")
  filteredDataSub <- filteredData[,index]
  data <- reshape(filteredDataSub, idvar=c("ActivityStartDateGiven","site","HydrologicEvent","HydrologicCondition"),timevar = "USGSPCode", direction="wide",sep="_")
  
  filteredPcode1 <- filteredData[filteredData$USGSPCode == filteredData$USGSPCode[2],]
  endDate <- setNames(filteredPcode1$ActivityEndDateGiven, filteredPcode1$ActivityStartDateGiven)

  
  data$ActivityEndDateGiven <- endDate[as.character(data$ActivityStartDateGiven)]
  row.names(data) <- NULL
  
  data <- data[,c(1:2,ncol(data),3:(ncol(data)-1))]
  return(data)  
}
