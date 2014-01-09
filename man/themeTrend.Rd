\name{themeTrend}
\alias{themeTrend}
\title{Graph changes in themes over time, given a GKG subset.}
\usage{
themeTrend(df, themes, location, overlay = TRUE, returndata = FALSE,
  span = 0.3)
}
\arguments{
  \item{df}{A subset of the Global Knowledge Graph,
  probably a country \code{gkg.df}}

  \item{themes}{A vector of themes from GKG.}

  \item{span}{The smoothing factor for the loess curve.
  Default is 0.3}

  \item{location}{A location, potentially more specific
  than the data frame subset.}

  \item{overlay}{Should the lines be plotted on the same
  graph or separate?}

  \item{returndata}{If true, returns the raw data and does
  not plot anything}
}
\value{
theme.counts A data frame containing number of events per
day per theme.
}
\description{
This takes a GKG dataframe and a list of themes and plots
the mentions of the themes over time.
}
\examples{
gkg <- read.csv("gkg.csv")
mex <- gkg[grep("Mexico", gkg$LOCATIONS),]
themeTrends(mex, c("CRIME_CARTELS", "SECURITY_SERVICES", "KILL"), location="Mexico")
}
\keyword{GDELT,}
\keyword{gdeltr}

