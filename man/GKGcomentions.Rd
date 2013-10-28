\name{GKGcomentions}
\alias{GKGcomentions}
\title{Given a subsetted dataframe from the Global Knowledge Graph, return a df with co-mentions.}
\usage{
  GKGcomentions(gkg.df, type)
}
\arguments{
  \item{gkg.df}{A subset of the Global Knowledge Graph
  \code{gkg.df}}

  \item{type}{Data types to subset: "themes", "persons",
  "organizations", "countries", or "placenames".
  \code{gkg.df}}
}
\value{
  countries.df A data frame containing count information.
}
\description{
  This takes a GKG dataframe (or subset thereof) returns a
  dataframe with all co-mentioned entities of the desired
  type listed on the same row. This is designed for export
  to social network analysis software.  Run the output
  through \code{write.gephi} if needed. New feature: uses
  \code{nameFixer} to standardize people names.
}
\examples{
ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
ieds.orgs <- GKGcomentions(ieds, type="organizations")
}
\keyword{GDELT,}
\keyword{gdeltr}

