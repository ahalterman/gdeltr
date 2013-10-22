\name{gDate}
\alias{gDate}
\title{Given a vector of 8 digit dates (yyyymmdd), returns a date obj in "yyyy-mm-dd"}
\usage{
  gDate(date.vector)
}
\arguments{
  \item{date.vector}{A vector of the SQLDATE column from
  GDELT, in form yyyymmdd \code{date.vector}}
}
\value{
  newdate A vector of class date with "yyyy-mm-dd" format
  \code{newdate}
}
\description{
  This comes up a lot working with GDELT. Make sure you
  only pass it a vector, not the whole data frame!
}
\examples{
R code here showing how your function works
}
\keyword{GDELT,}
\keyword{gdeltr}

