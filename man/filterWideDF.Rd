\name{filterWideDF}
\alias{filterWideDF}
\title{Filter the wide dataframe by parameter code}
\usage{
filterWideDF(wideDF, PCode)
}
\arguments{
  \item{wideDF}{data.frame}

  \item{PCode}{string 5 digit USGS parameter code (can be a
  vector)}
}
\value{
DF dataframe
}
\description{
Filter the wide dataframe by parameter code, codes can come
in individually as a simple string, or in a character
vector
}
\examples{
genericCensoringValue <- function(qualifier,value, detectionLimit){
   valueToUse <- ifelse("<" == qualifier, detectionLimit, value)
   return(valueToUse)
}
wideDF <- wideGLRIData(filterGLRIData(QWPortalGLRI,genericCensoringValue))
temperature <- filterWideDF(wideDF,"00010")
multipleData <- filterWideDF(wideDF,c("00010","62818"))
}
\keyword{stat}
\keyword{summary}

