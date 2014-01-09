\name{toneTrend}
\alias{toneTrend}
\title{Graph changes in tone over time, given a GKG subset.}
\usage{
toneTrend(df, objects, type, location, overlay = TRUE, span = 0.3,
  returndata = FALSE)
}
\arguments{
  \item{df}{A subset of the Global Knowledge Graph,
  probably a country \code{gkg.df}}

  \item{object}{A vector of your entities of interest
  (persons, themes, or organizations.)}

  \item{type}{persons, themes, or organizations? Only works
  for "theme" now.}

  \item{location}{A location, potentially more specific
  than the data frame subset.}

  \item{overlay}{Should the lines be plotted on the same
  graph or separate?}

  \item{returndata}{If true, returns the raw data and does
  not plot anything}

  \item{span}{How much smoothing on the loess curve?}
}
\value{
theme.counts A data frame containing number of events per
day per theme.
}
\description{
This takes a GKG dataframe and organizations/themes/people
of interest and returns the tone of daily news coverage of
that entity. Right now it only does themes.
}
\examples{
gkg <- read.csv("gkg.csv")
mex <- gkg[grep("Mexico", gkg$LOCATIONS),]
toneTrend(mex, c("CRIME_CARTELS", "SECURITY_SERVICES", "KILL"), type="theme", location="Mexico")
}
\keyword{GDELT,}
\keyword{gdeltr}

