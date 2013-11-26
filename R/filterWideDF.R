#' Filter the wide dataframe by parameter code
#'
#' Filter the wide dataframe by parameter code, codes can come in individually as a simple string, or in a character vector
#'
#' @param wideDF data.frame 
#' @param PCode string 5 digit USGS parameter code (can be a vector)
#' @keywords stat summary
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' wideDF <- wideGLRIData(filterGLRIData(QWPortalGLRI,genericCensoringValue))
#' temperature <- filterWideDF(wideDF,"00010")
#' multipleData <- filterWideDF(wideDF,c("00010","62818"))
filterWideDF <- function(wideDF, PCode){

  colNames <- names(wideDF)
  colNamesSpl <- sapply(strsplit(colNames, "_"),function(x)x[2])
  index <- which(colNamesSpl %in% PCode)
  
  firstQual <- grep("qualifier",names(wideDF))[1]-1
  
  index <- c(1:firstQual, index)
  DF1 <- wideDF[,index]
  colNames2 <- names(DF1)
  colNames2Split <- sapply(strsplit(colNames2, "_"),function(x)x[1])
  index2 <- which(colNames2Split %in% "qualifier")
  DF <- DF1[which(rowSums(!is.na(DF1[index2])) > 0),]
  
  return(DF)
}