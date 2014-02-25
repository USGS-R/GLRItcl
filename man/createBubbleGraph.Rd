\name{createBubbleGraph}
\alias{createBubbleGraph}
\title{Create overview bubble graph of GLRI data}
\usage{
createBubbleGraph(DF, xCol = "class", yCol = "site", colCol = "max",
  radCol = "percentDetect", colorTitle = "Maximum Concentration",
  radiusTitle = "Frequency of detection [\%]", colorScale = c(0.01, 0.1, 1,
  10, Inf), radiusScale = c(0, 20, 40, 60, 80, 100), countCol = "count",
  savePDF = FALSE, fileName = "bubble.pdf", colorGradient = FALSE,
  radiusGradient = TRUE)
}
\arguments{
  \item{DF}{data.frame}

  \item{xCol}{string column from DF defining x axis}

  \item{yCol}{string column from DF defining y axis}

  \item{colCol}{string column from DF defining color}

  \item{radCol}{string column from DF defining radius}

  \item{colorTitle}{string title for color legend}

  \item{radiusTitle}{string title for radius legend}

  \item{colorScale}{vector scale for color}

  \item{radiusScale}{vector scale for radius}

  \item{countCol}{string column from DF defining count}

  \item{savePDF}{logical}

  \item{fileName}{string}

  \item{colorGradient}{logical use a linear color gradient
  or discrete colors (number set by colorScale)}

  \item{radiusGradient}{logical use a linear radius
  gradient or discrete radii (number set by radiusScale)}
}
\description{
Create overview bubble graph of GLRI data. Testing Mac.
}
\examples{
genericCensoringValue <- function(qualifier,value, detectionLimit){
   valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
   return(valueToUse)
 }
filteredData <- filterGLRIData(QWPortalGLRI, genericCensoringValue)
wideDF <- wideGLRIData(filteredData)
keyDF <- pcodeINFO
classCol <- "class"
pCodeCol <- "parameter_cd"
dataByClass <- PCodeClassSummary(wideDF,keyDF,pCodeCol,classCol)
summaryByClass <- statClassSummary(dataByClass)
createBubbleGraph(summaryByClass, xCol="class", yCol="site", colCol="max", radCol="percentDetect",colorTitle="Maxiumum",radiusTitle="Detection",countCol="count")
}
\keyword{stat}
\keyword{summary}

