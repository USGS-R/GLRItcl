\name{getGLRIData}
\alias{getGLRIData}
\title{Get QW data for multiple sites}
\usage{
  getGLRIData(siteNumber, startDate, OWC = TRUE,
    pCodes = NULL)
}
\arguments{
  \item{siteNumber}{string vector of just USGS site ID
  numbers}

  \item{startDate}{string date in format yyyy-mm-dd}

  \item{OWC}{logical if using OWC, parameter codes taken
  from pcodeINFO dataset}

  \item{pCodes}{character vector if not using OWC, pCodes
  list}
}
\value{
  dataframe
}
\description{
  Gets the basic data from the water quality portal.
}
\examples{
OWCdata <- getGLRIData(c('04010500','04024000'), '2013-01-01',OWC=TRUE)
nonOWCdata <- getGLRIData(c('04010500','04024000'), '2013-01-01',OWC=FALSE, c('00010','00300'))
}
\keyword{data}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

