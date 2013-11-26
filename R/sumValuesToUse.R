#' Filter the wide dataframe by parameter code
#'
#' Filter the wide dataframe by parameter code, codes can come in individually as a simple string, or in a character vector
#'
#' @param filteredDF data.frame 
#' @keywords stat summary
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' wideDF <- wideGLRIData(filterGLRIData(QWPortalGLRI,genericCensoringValue))
#' PCodePLASTICIZER <- unique(pcodeINFO$parameter_cd["PLASTICIZER" == pcodeINFO$class & !is.na(pcodeINFO$class)])
#' filteredDF <- filterWideDF(wideDF,PCodePLASTICIZER)
#' sumValuesToUse(filteredDF)
sumValuesToUse <- function(filteredDF){
  
  colNames <- names(filteredDF)
  colNamesSpl <- sapply(strsplit(colNames, "_"),function(x)x[1])
  index <- which(colNamesSpl == "valueToUse")
  subDF <- filteredDF[,index]
  
  if(is.data.frame(subDF)){
    sumOfValues <- rowSums(subDF, na.rm = TRUE)
  } else {
    sumOfValues <- subDF
  }
  
  index2 <- which(colNamesSpl == "qualifier")
  subDF2 <- filteredDF[,index2]  
  
  detected <- ifelse(subDF2 == "<",0,1)  # If the qualifier is "<", 0, otherwise 1
  detected2 <- ifelse(subDF == 0,0,1) # if the valueToUse is 0, 0 otherwise 1
  detected3 <- detected + detected2 # if either 
  detected <- ifelse(detected3 > 1,1,0)
  
  if(is.data.frame(subDF2)){
    percentDetected <- ifelse(rowSums(detected, na.rm = TRUE)>0,100,0)
#     percentDetected <- 100*rowMeans(detected, na.rm = TRUE)
  } else {
    percentDetected <- ifelse(detected>0,100,0)
#     percentDetected <- 100*detected
  }
  firstQual <- grep("qualifier",names(filteredDF))[1]-1
  DF <- cbind(filteredDF[,1:firstQual],sumOfValues, percentDetected)
  
  return(DF)
}