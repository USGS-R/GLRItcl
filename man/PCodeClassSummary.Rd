\name{PCodeClassSummary}
\alias{PCodeClassSummary}
\title{Filter the wide dataframe by parameter code}
\usage{
PCodeClassSummary(wideDF, keyDF, pCodeCol, classCol, merge = TRUE)
}
\arguments{
  \item{wideDF}{data.frame}

  \item{keyDF}{dataframe}

  \item{pCodeCol}{string}

  \item{classCol}{string}

  \item{merge}{logical}
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
keyDF <- pcodeINFO
classColumn <- "class"
pCodeColumn <- "parameter_cd"
pCodeSummary <- PCodeClassSummary(wideDF,keyDF,pCodeColumn,classColumn)
}
\keyword{stat}
\keyword{summary}

