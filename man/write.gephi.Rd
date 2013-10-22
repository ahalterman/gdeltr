\name{write.gephi}
\alias{write.gephi}
\title{Wrapper for write.table for outputting from the GKG to Gephi}
\usage{
  write.gephi(gkg.df, filename, type)
}
\arguments{
  \item{gkg.df}{A dataframe to export to gephi
  \code{gkg.df}}

  \item{filename}{The name for the \code{gkg.df}}

  \item{type}{node or edge?  Node won't put quotes on
  anything except the first column}
}
\value{
  gkg.df A semicolon seperated file with quotes.
}
\description{
  Specifically, it adds quotes to prevent extra splitting,
  removes row/col names, and saves with a semicolon
  separator. Obvs., it's undirected. If it's a node list,
  the nodes MUST be in a column labeled "ID".
}
\examples{
R code here showing how your function works
}
\keyword{GDELT,}
\keyword{gdeltr}

