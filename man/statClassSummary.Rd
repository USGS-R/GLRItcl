\name{statClassSummary}
\alias{statClassSummary}
\title{Calculate and summarize stats on GLRI data}
\usage{
  statClassSummary(dataByClass)
}
\arguments{
  \item{dataByClass}{data.frame}
}
\value{
  DF dataframe
}
\description{
  Filter the raw data into a simple dataframe.
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
statClassSummary(dataByClass)
}
\keyword{stat}
\keyword{summary}

