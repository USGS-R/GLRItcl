\name{PlotPriorities}
\alias{PlotPriorities}
\title{Function to help prioritization of sites based on available data}
\usage{
  PlotPriorities(df, parm, xLabelParm, statParm,
    countThresh, concThresh, plotTitle, ylab,
    subsetVar = NA, subsetValue = NA, logy = FALSE,
    addY = 0)
}
\arguments{
  \item{df}{Dataframe with observations and conditioning
  variable}

  \item{parm}{Parameter in df to be plotted}

  \item{xLabelParm}{Parameter in df to be used as a
  conditioning variable for graphing}

  \item{statParm}{Summary statistic to be used to order
  xLabelParm for the x-axis. Usually this is the montiroing
  site short name}

  \item{countThresh}{Threshold number of observations for
  determining high-count compared to low-count sites}

  \item{concThresh}{Threshold concentration for
  prioritization and coloring x-axis labels}

  \item{plotTitle}{Main title of the plot}

  \item{ylab}{y-axis label}

  \item{subsetVar}{If df is to be subset, this is the
  column name to be used. Commonly a character or factor}

  \item{subsetValue}{The value of subsetVar used for
  subsetting df}

  \item{logy}{Boolean variable to designate log y-axis.
  TRUE if log, or FALSE if linear (the default).}

  \item{addY}{Value to add to parm values before plotting
  or "min10" to find the minimum nonzero value, divide by
  10, and add to the parm values. This is useful to plot on
  log scale when there are zero values.}
}
\value{
  NULL
}
\description{
  Combination boxplot and points graphing function. This
  orders the boxplots and points first by the number of
  data points per site and then by the mean for each site.
}
\examples{
x<-1
}
\keyword{boxplot}
\keyword{counts}
\keyword{ordered}
\keyword{prioritize}

