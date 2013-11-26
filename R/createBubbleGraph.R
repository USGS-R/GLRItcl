#' Create overview bubble graph of GLRI data
#'
#' Create overview bubble graph of GLRI data. Testing Mac.
#'
#' @param DF data.frame
#' @param xCol string column from DF defining x axis
#' @param yCol string column from DF defining y axis
#' @param colCol string column from DF defining color
#' @param radCol string column from DF defining radius
#' @param colorTitle string title for color legend
#' @param radiusTitle string title for radius legend
#' @param colorScale vector scale for color
#' @param radiusScale vector scale for radius
#' @param countCol string column from DF defining count
#' @param savePDF logical
#' @param fileName string 
#' @param colorGradient logical use a linear color gradient or discrete colors (number set by colorScale)
#' @param radiusGradient logical use a linear radius gradient or discrete radii (number set by radiusScale)
#' @keywords stat summary
#' @return NULL 
#' @export
#' @examples
#' genericCensoringValue <- function(qualifier,value, detectionLimit){
#'    valueToUse <- ifelse("<" == qualifier, detectionLimit, value)    
#'    return(valueToUse)
#'  }
#' filteredData <- filterGLRIData(QWPortalGLRI, genericCensoringValue)
#' wideDF <- wideGLRIData(filteredData)
#' keyDF <- pcodeINFO
#' classCol <- "class"
#' pCodeCol <- "parameter_cd"
#' dataByClass <- PCodeClassSummary(wideDF,keyDF,pCodeCol,classCol)
#' summaryByClass <- statClassSummary(dataByClass)
#' createBubbleGraph(summaryByClass, xCol="class", yCol="site", colCol="max", radCol="percentDetect",colorTitle="Maxiumum",radiusTitle="Detection",countCol="count") 
createBubbleGraph <- function(DF, xCol="class", yCol="site", 
    colCol="max", radCol="percentDetect", 
    colorTitle="Maximum Concentration", radiusTitle="Frequency of detection [%]",
    colorScale = c(0.01,0.1,1,10,Inf), radiusScale = c(0,20,40,60,80,100),
    countCol ="count",
    savePDF = FALSE, fileName = "bubble.pdf", colorGradient=FALSE,radiusGradient=TRUE){

  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    s <- tolower(s)
    retString <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    retString <- ifelse(retString == "Pah", "PAHs", retString)
  }
  
  wordWrap <- function(x,len){
    paste(strwrap(x,width=len),collapse="\n")
  }
  
  colors<-colorRampPalette(c("white","blue"))
  
  DF$colorLabel <- cut(DF[[colCol]],colorScale,include.lowsest=TRUE,right=TRUE)
  DF$colorCode <- cut(DF[[colCol]],colorScale,include.lowsest=TRUE,right=FALSE,labels=FALSE)
  DF$radiusLabel <- cut(DF[[radCol]],radiusScale,include.lowsest=TRUE,right=TRUE)
  DF$radiusCode <- cut(DF[[radCol]],radiusScale,include.lowsest=TRUE,right=FALSE,labels=FALSE)
  
  if(colorGradient){
    my.colors<-colors(100)
    DF$plotcolor <- my.colors[1+round(100*DF[[colCol]]/max(DF[[colCol]]))]    
  } else {
    my.colors<-colors(length(colorScale))
    DF$plotcolor <- my.colors[DF$colorCode]    
  }
  
  if(radiusGradient){
    DF$circles=DF[[radCol]]/max(DF[[radCol]])
  } else {
    DF$circles=DF$radiusCode
  }

  xFactor <- factor(sapply(DF[[xCol]],.simpleCap)) 
  yFactor <- factor(DF[[yCol]])

  subsubDF <- DF[DF[[xCol]] == DF[[xCol]][1],]
  
  subDF <- data.frame(site=subsubDF[[yCol]], count=subsubDF[[countCol]])
  
  count <- subDF$count
  names(count) <- subDF$site
  
  if (savePDF) pdf(file=fileName)

    par(oma=c(0.25,0.25,1,0))
    symbols(xFactor,yFactor,
            circles=DF$circles,inches=0.1,
            bty="n",bg=DF$plotcolor,xlab="",ylab="",xaxt="n",yaxt="n")

    text(0, yFactor[1:length(count)], count[as.character(yFactor[1:length(count)])], cex=0.4) 
  
    axis(2, at=c(1:length(levels(yFactor))),
         levels(yFactor),
         line=-0.5,
         las=2,tck=0,cex.axis=0.6,col="blue",lwd=2)
    
    topLabels <- c("Number of Samples", levels(xFactor))  
    topLabels <- sapply(topLabels, wordWrap,len=18)
    
    axis(3,at=c(0:length(levels(xFactor))),line=-1.5,
         labels=topLabels,
         las=2,tck=0,cex.axis=0.6,col="blue",lwd=2)
    
    legend_size<- as.numeric(levels(factor(DF$radiusCode)))*5
  
    legend(2.5,0,title=colorTitle,bty="n",levels(DF$colorLabel),
           pt.bg=my.colors,pt.cex=legend_size[2]*0.1,pch=21,cex=0.6,xpd=TRUE)
  
    legend(7.5,0,title=radiusTitle,bty="n",levels(DF$radiusLabel),
           pt.cex=legend_size*0.1,pch=21,pt.bg=my.colors[2],cex=0.6,xpd=TRUE)
  
    rect(2, -1.2*length(levels(DF$colorLabel)), 11.5, 0 ,border="blue", lwd=2,xpd=TRUE)

  if (savePDF) dev.off()

}