\name{GKGextractcameo}
\alias{GKGextractcameo}
\title{Extract CAMEO events from GKG}
\usage{
  GKGextractcameo(df, justvector = TRUE)
}
\arguments{
  \item{df}{A dataframe of GKG namesets}

  \item{justvector}{Return vector of ID numbers instead of
  actual data frame?}
}
\value{
  gdelt.df A vector of all linked CAMEO event IDs
}
\description{
  From a GKG subset dataframe, return a dataframe of all
  linked CAMEO event IDs. If \code{justvector=TRUE}, the
  fuction will return only a vector of linked CAMEO event
  IDs rather than the complete dataframe of all matching
  events.
}
\examples{
cameos.events <- GKGextractcameo(mexico.cartels)
}
\keyword{GDELT,}
\keyword{gdeltr}

