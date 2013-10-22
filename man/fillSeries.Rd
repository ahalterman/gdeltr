\name{fillSeries}
\alias{fillSeries}
\title{Fills in missing dates in a data frame of GDELT events for plotting or time series analysis}
\usage{
  fillSeries(df, begin.date = "2000-01-01",
    end.date = "2013-09-30", date.column = "SQLDATE",
    extraclean = FALSE)
}
\arguments{
  \item{df}{A GDELT dataframe.  \code{df}}

  \item{begin.date}{The earliest date.  Defaults to Jan 1,
  2000.  \code{begin.date}}

  \item{end.date}{The last date.  Defaults to Sept 30,
  2013.  \code{end.date}}

  \item{date.column}{The name of the column containing
  dates.  Defaults to "SQLDATE" \code{date.column}}
}
\value{
  df2 A
}
\description{
  Fills in missing dates in a data frame of GDELT events
  for plotting or time series analysis
}
\examples{
R code here showing how your function works
}
\keyword{GDELT,}
\keyword{gdeltr}

