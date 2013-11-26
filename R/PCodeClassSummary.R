#' Filter the wide dataframe by parameter code
#'
#' Filter the wide dataframe by parameter code, codes can come in individually as a simple string, or in a character vector
#'
#' @param wideDF data.frame 
#' @param keyDF dataframe
#' @param pCodeCol string
#' @param classCol string
#' @param merge logical
#' @keywords stat summary
#' @return DF dataframe 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
#'    return(valueToUse)
#' }
#' wideDF <- wideGLRIData(filterGLRIData(QWPortalGLRI,genericCensoringValue))
#' keyDF <- pcodeINFO
#' classColumn <- "class"
#' pCodeColumn <- "parameter_cd"
#' pCodeSummary <- PCodeClassSummary(wideDF,keyDF,pCodeColumn,classColumn)
PCodeClassSummary <- function(wideDF,keyDF,pCodeCol,classCol,merge=TRUE){
  
  classes <- unique(keyDF[[classCol]])
  classes <- classes[!is.na(classes)]
    
  firstQual <- grep("qualifier",names(wideDF))[1]-1
  
  DF <- wideDF[,1:firstQual]  
  
  for (i in 1:length(classes)){
    
    subPCode <- unique(keyDF[[pCodeCol]][classes[i] == keyDF[[classCol]]])
    subPCode <- subPCode[!is.na(subPCode)]
    filteredDF <- filterWideDF(wideDF,subPCode)

    if (nrow(filteredDF) > 0){
      subDF <- sumValuesToUse(filteredDF)
      colnames(subDF) <- c(colnames(subDF)[1:firstQual],paste(colnames(subDF)[firstQual+1],classes[i],sep="_"),paste(colnames(subDF)[firstQual+2],classes[i],sep="_"))
      if(merge){
        DF <- merge(DF,subDF,all=TRUE)    
      } else {
        DF <- cbind(DF,subDF[,(ncol(subDF)-1):ncol(subDF)])    
      }
        
    }
  }
  
  colNames2 <- names(DF)
  colNames2Split <- sapply(strsplit(colNames2, "_"),function(x)x[1])
  index2 <- which(colNames2Split %in% "percentDetected")
  
  DFtest <- DF[which(rowSums(is.na(DF[index2])) != length(index2)),]
  
  return(DFtest)
}