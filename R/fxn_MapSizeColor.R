#' Mapping data with variable size and color to define spatial data
#' 
#' Mapping routine that displays spatial data variability by size and color differences.
#' over layers with political boundaries, hydrologic polygons, and hydrologic lines.
#' 
#' @param df Dataframe with two columns that have data to define symbol size, color,
#' latitude, and longitude of points to be plotted
#' @param colorVar Column name in df to define symbol color
#' @param sizeVar Column name in df to define symbol size
#' @param latVar Column name in df to define latitude
#' @param lonVar Column name in df to define longitude
#' @param sizeThresh1 Low threshold value of sizeVar for defining bins
#' @param sizeThresh2 High  threshold value of sizeVar for defining bins
#' @param politicalBounds Shapefile of class "SpatialPolygonsDataFrame" for 
#' defining political boundaries
#' @param hydroPolygons Shapefile of class "SpatialPolygonsDataFrame" for 
#' defining hydrologic polygons (lakes)
#' @param hydroLines shapefile of class "SpatialLinesDataFrame" for 
#' defining hydrologic lines (rivers/streams)
#' @param xmin Left longitudinal boundary for plotting
#' @param xmax Right longitudinal boundary for plotting
#' @param ymin Bottom latitudinal boundary for plotting
#' @param ymax Top latitudinal boundary for plotting
#' @param col1 Symbol color for 0-0.25 quantile bin
#' @param col2 Symbol color for 0.25-0.5 quantile bin
#' @param col3 Symbol color for 0.5-0.75 quantile bin
#' @param col4 Symbol color for 0.75-1.0 quantile bin
#' @param xleft Placement of left side of legend box (min latitude)
#' @param xright Placement of right side of legend box (min latitude)
#' @param ybottom  Placement of bottom side of legend box (min longitude)
#' @param ytop  Placement of top side of legend box (min longitude)
#' @keywords map spatial size color
#' @return NULL
#' @import rgdal
#' @import sp
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
#' OWCSummary <- statClassSummary(dataByClass)
#' OWCSummary$STAID <- sub("USGS-","",OWCSummary$site)
#' rownames(stationINFO) <- stationINFO$STAID
#' OWCSummary$lat.dd <- stationINFO[OWCSummary[,"STAID"],"dec.lat.va"]
#' OWCSummary$lon.dd <- stationINFO[OWCSummary[,"STAID"],"dec.long.va"]
#' plotClass="HERBICIDE"
#' df <- subset(OWCSummary, class==plotClass)
#' colorVar <- "mean"
#' sizeVar <- "count"
#' latVar <- "lat.dd"
#' lonVar <- "lon.dd"
#' politicalBounds <- shape_poliboundsClip
#' hydroPolygons <- subShape_hydropolyClip
#' hydroLines <- shape_hydrolineClip
#' xmin <- -96.5
#' xmax <- -72
#' ymin <- 40.5
#' ymax <- 49.5
#' xleft <- -95
#' ybottom <- 40.4
#' xright <- -90.8
#' ytop <- 45.3
#' col1 <- "tan"
#' col2 <- "orange3"
#' col3 <- "orangered1"
#' col4 <- "orangered4"
#' sizeThresh1 <- 2
#' sizeThresh2 <- 14
#' MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,sizeThresh1,sizeThresh2,
#'              politicalBounds,hydroPolygons,hydroLines,
#'              xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom)
MapSizeColor <- function(df,colorVar,sizeVar,latVar,lonVar,
                         sizeThresh1,sizeThresh2,
                         politicalBounds,hydroPolygons,hydroLines,
                         xmin,xmax,ymin,ymax,
                         col1="tan",col2="orange3",col3="orangered1",col4="orangered4",
                         xleft,xright,ytop,ybottom){

  #set plot parameters
  par( mar=c(0,0,1,0), new = FALSE,xpd=NA)#,mgp=c(3,0.1,0))
  
  #Choose plot color bins: 
  #Use 0.25, 0.5, and 0.75 quantiles of non-zero values to define bins
  which(df[,colorVar] != 0)
  binThresh <- quantile(df[which(df[,colorVar] != 0),colorVar],c(0.25,0.5,0.75))
  binCol <- c(col2,col3,col4)
  plotSymbol <- 21 
  plotSize <- ifelse(df[,sizeVar] < sizeThresh1,1,1.5)
  plotSize <- ifelse(df[,sizeVar] >sizeThresh2,2,plotSize)
  
  fillCol <- rep(col1,dim(df)[1])
  for (i in 1:length(binThresh)) fillCol <- ifelse(df[,colorVar] > binThresh[i],binCol[i],fillCol)
  plot(politicalBounds,col="gray90",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  plot(hydroPolygons,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax),add=TRUE)#
  lines(hydroLines,col="lightskyblue2",xlim=c(xmin,xmax),ylim=c(ymin,ymax))#
  plot(politicalBounds,add=TRUE)
  points(df[,lonVar], df[,latVar],pch=plotSymbol, col="black",cex=plotSize,bg=fillCol)
  mtext(plotClass[1],side=3,line=-4,outer=TRUE,font=2,cex=1.3)
  
  
  legendTextCex <- 0.9
  rect(xleft=xleft,ybottom=ybottom,xright=xright,ytop=ytop,col="white",)
  legend(x=xleft+0.2,y=ytop-0.9,c("1-2 samples", "3-14 samples","> 14 samples"),bty="n",
         #       title=expression(bold("Number of Samples")),
         pch=c(21),pt.cex=c(1,1.5,2),bg="white",pt.bg="orange3")
  binThresh <- round(binThresh,3)
  legendText= c(paste("<",binThresh[1]),
                paste(binThresh[1],"-",binThresh[2]),
                paste(binThresh[2],"-",binThresh[3]),
                paste(binThresh[3],"-",round(max(df[,colorVar]),3)))
  legendCol = binCol
  startText <- c(xleft+0.2,ytop-0.3)
  text("Size of symbol",x=startText[1],y=startText[2],font=2,pos=4,cex=legendTextCex)
  text("indicates number",x=startText[1],y=startText[2]-0.3,font=2,pos=4,cex=legendTextCex)
  text("of samples",x=startText[1],y=startText[2]-0.6,font=2,pos=4,cex=legendTextCex)
  
  startText <- c(xleft+0.2,ytop-2.4)
  text("Color of symbol",x=startText[1],y=startText[2],font=2,pos=4,cex=legendTextCex)
  text("indicates",x=startText[1],y=startText[2]-0.3,font=2,pos=4,cex=legendTextCex)
  text(expression(bold(paste("Concentration (",mu,"g/L)",sep=""))),x=startText[1],y=startText[2]-0.6,font=2,pos=4,cex=legendTextCex)
  
  legend(x=xleft+0.2,y=ytop-3.,legendText,pt.bg=c("tan",binCol),pch=plotSymbol,bg="white",pt.cex=1.5,bty="n")
}

