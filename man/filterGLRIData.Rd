\name{filterGLRIData}
\alias{filterGLRIData}
\title{Filter the raw data into long dataframe.}
\usage{
filterGLRIData(rawData, valueToUseFunction)
}
\arguments{
  \item{rawData}{data.frame}

  \item{valueToUseFunction}{function that has input
  qualifier, value, detection limit, and outputs
  valueToUse}
}
\value{
DF dataframe
}
\description{
Filter the raw data into a simple dataframe. Output
dataframe is in long format.
}
\examples{
genericCensoringValue <- function(qualifier,value, detectionLimit){
   valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
   return(valueToUse)
}
filteredOWC <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
}
\keyword{filter}

