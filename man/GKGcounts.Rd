\name{GKGcounts}
\alias{GKGcounts}
\title{Given a dataframe of "Counts" information from the GKG}
\usage{
  GKGcounts(gkg)
}
\arguments{
  \item{gkg}{A subset dataframe of the GKG.}
}
\value{
  counts A data frame containing information from the
  \code{Counts} column.
}
\description{
  The Global Knowledge Graph contains two elements, the
  "Counts" file, containing information on the numbers of
  people killed, affected, etc. every day by location. The
  second file, the "Graph file", contains the associated
  themes, organizations, people, and locations.  Filtering,
  especially by themes, is very useful, but much of the
  useable information in the GKG is in the Counts file.
  This function will return the Counts file, nicely
  formatted (it's \code{;} and \code{#} separated, which is
  a hassle), in no particular order.  It loses the date and
  theme information, though, which is the next room for
  improvement.
}
\examples{
# Say we were interested in the number of people killed by mines/IEDs.
ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
ieds.counts <- GKGcounts(ieds)
}
\keyword{GDELT,}
\keyword{gdeltr}

