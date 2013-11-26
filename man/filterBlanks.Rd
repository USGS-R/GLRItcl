\name{filterBlanks}
\alias{filterBlanks}
\title{Filter the blanks and quality control sample data}
\usage{
  filterBlanks(rawData)
}
\arguments{
  \item{rawData}{data.frame from a water quality portal
  pull}
}
\value{
  rawData_useful dataframe
}
\description{
  Filter the blanks and quality control sample data.
}
\examples{
QWPortalGLRI <- QWPortalGLRI
noBlanksData <- filterBlanks(QWPortalGLRI)
}
\keyword{filter}

