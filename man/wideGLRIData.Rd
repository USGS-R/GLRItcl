\name{wideGLRIData}
\alias{wideGLRIData}
\title{Convert long GLRI dataframe to wide}
\usage{
wideGLRIData(filteredData)
}
\arguments{
  \item{filteredData}{data.frame}
}
\value{
DF dataframe
}
\description{
Filter the filtered data into a wide format.
}
\examples{
genericCensoringValue <- function(qualifier,value, detectionLimit){
   valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
   return(valueToUse)
}
filteredData <- filterGLRIData(QWPortalGLRI,genericCensoringValue)
wideGLRIData(filteredData)
}
\keyword{filter}

