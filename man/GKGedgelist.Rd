\name{GKGedgelist}
\alias{GKGedgelist}
\title{Convert a ragged data frame into an edgelist}
\usage{
  GKGedgelist(df, max.connections = 30)
}
\arguments{
  \item{df}{A subset of the GKG \code{df}}

  \item{max.connections}{How many columns to include? Set a
  number or "all". Default is 30.}
}
\value{
  edgelist A data frame with two columns containing the two
  nodes defining each edge.
}
\description{
  Because GKG's persons, organizations, etc. fields
  countain varying numbers of elements, converting them to
  a data frame will produce a ragged data frame (rows with
  different numbers of columns, albeit padded with NAs.
  Gephi can import ragged data frames, thought not if you
  plan to include node attributes.  This function will take
  a ragged data frame and return a edgelist data frame (2
  columns, lots of rows).
}
\details{
  Right now, even with the apply setup (instead of the
  awful earlier for-loop) it's still really, really slow.

  Some namesets contain hundreds of names.  To increase
  speed at the loss of some connections, you can limit the
  number of columns that are included.  By default, this is
  set to 30.
}
\examples{
corruption<- gkg[grep("CORRUPTION", gkg$THEMES),]
corruption <- GKGcomentions(corruption, type="persons")
corruption.edgelist <- GKGedgelist(corruption, max.connections=40)
}
\keyword{GDELT,}
\keyword{gdeltr}

