\name{MapSizeColor}
\alias{MapSizeColor}
\title{Mapping data with variable size and color to define spatial data}
\usage{
  MapSizeColor(df, colorVar, sizeVar, latVar, lonVar,
    sizeThresh1, sizeThresh2, politicalBounds,
    hydroPolygons, hydroLines, xmin, xmax, ymin, ymax,
    col1 = "tan", col2 = "orange3", col3 = "orangered1",
    col4 = "orangered4", xleft, xright, ytop, ybottom)
}
\arguments{
  \item{df}{Dataframe with two columns that have data to
  define symbol size, color, latitude, and longitude of
  points to be plotted}

  \item{colorVar}{Column name in df to define symbol color}

  \item{sizeVar}{Column name in df to define symbol size}

  \item{latVar}{Column name in df to define latitude}

  \item{lonVar}{Column name in df to define longitude}

  \item{sizeThresh1}{Low threshold value of sizeVar for
  defining bins}

  \item{sizeThresh2}{High threshold value of sizeVar for
  defining bins}

  \item{politicalBounds}{Shapefile of class
  "SpatialPolygonsDataFrame" for defining political
  boundaries}

  \item{hydroPolygons}{Shapefile of class
  "SpatialPolygonsDataFrame" for defining hydrologic
  polygons (lakes)}

  \item{hydroLines}{shapefile of class
  "SpatialLinesDataFrame" for defining hydrologic lines
  (rivers/streams)}

  \item{xmin}{Left longitudinal boundary for plotting}

  \item{xmax}{Right longitudinal boundary for plotting}

  \item{ymin}{Bottom latitudinal boundary for plotting}

  \item{ymax}{Top latitudinal boundary for plotting}

  \item{col1}{Symbol color for 0-0.25 quantile bin}

  \item{col2}{Symbol color for 0.25-0.5 quantile bin}

  \item{col3}{Symbol color for 0.5-0.75 quantile bin}

  \item{col4}{Symbol color for 0.75-1.0 quantile bin}

  \item{xleft}{Placement of left side of legend box (min
  latitude)}

  \item{xright}{Placement of right side of legend box (min
  latitude)}

  \item{ybottom}{Placement of bottom side of legend box
  (min longitude)}

  \item{ytop}{Placement of top side of legend box (min
  longitude)}
}
\value{
  NULL
}
\description{
  Mapping routine that displays spatial data variability by
  size and color differences. over layers with political
  boundaries, hydrologic polygons, and hydrologic lines.
}
\examples{
genericCensoringValue <- function(qualifier,value, detectionLimit){
   valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
   return(valueToUse)
}
filteredData <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
wideDF <- wideGLRIData(filteredData)
keyDF <- pcodeINFO
classCol <- "class"
pCodeCol <- "parameter_cd"
dataByClass <- PCodeClassSummary(wideDF,keyDF,pCodeCol,classCol)
OWCSummary <- statClassSummary(dataByClass)
OWCSummary$STAID <- sub("USGS-","",OWCSummary$site)
rownames(stationINFO) <- stationINFO$STAID
OWCSummary$lat.dd <- stationINFO[OWCSummary[,"STAID"],"dec.lat.va"]
OWCSummary$lon.dd <- stationINFO[OWCSummary[,"STAID"],"dec.long.va"]
plotClass="HERBICIDE"
df <- subset(OWCSummary, class==plotClass)
colorVar <- "mean"
sizeVar <- "count"
latVar <- "lat.dd"
lonVar <- "lon.dd"
politicalBounds <- shape_poliboundsClip
hydroPolygons <- subShape_hydropolyClip
hydroLines <- shape_hydrolineClip
xmin <- -96.5
xmax <- -72
ymin <- 40.5
ymax <- 49.5
xleft <- -95
ybottom <- 40.4
xright <- -90.8
ytop <- 45.3
col1 <- "tan"
col2 <- "orange3"
col3 <- "orangered1"
col4 <- "orangered4"
sizeThresh1 <- 2
sizeThresh2 <- 14
MapSizeColor(df,colorVar,sizeVar,latVar,lonVar,sizeThresh1,sizeThresh2,
             politicalBounds,hydroPolygons,hydroLines,
             xmin,xmax,ymin,ymax,xleft=xleft,xright=xright,ytop=ytop,ybottom=ybottom)
}
\keyword{color}
\keyword{map}
\keyword{size}
\keyword{spatial}

