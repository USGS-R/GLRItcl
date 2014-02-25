\name{sumValuesToUse}
\alias{sumValuesToUse}
\title{Filter the wide dataframe by parameter code}
\usage{
sumValuesToUse(filteredDF)
}
\arguments{
  \item{filteredDF}{data.frame}
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
PCodePLASTICIZER <- unique(pcodeINFO$parameter_cd["PLASTICIZER" == pcodeINFO$class & !is.na(pcodeINFO$class)])
filteredDF <- filterWideDF(wideDF,PCodePLASTICIZER)
sumValuesToUse(filteredDF)
}
\keyword{stat}
\keyword{summary}

