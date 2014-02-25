\name{statSummary}
\alias{statSummary}
\title{Calculate and summarize stats on GLRI data}
\usage{
statSummary(filteredData)
}
\arguments{
  \item{filteredData}{data.frame}
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
statSummary(filteredData)
}
\keyword{stat}
\keyword{summary}

