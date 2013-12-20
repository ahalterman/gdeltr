\name{subsetEventCountry}
\alias{subsetEventCountry}
\title{Subset GDELT by a given EventRootCode and country name, returning lat/long for each event.}
\usage{
subsetEventCountry(event.root.code, country.name, min.date = 20000101)
}
\arguments{
  \item{event.root.code}{One of the 20 EventRootCodes in
  CAMEO/GDELT, including leading zero
  \code{event.root.code}}

  \item{country.name}{A normal English country name
  \code{country.name}}

  \item{min.date}{Furthest date back you want.
  \code{min.date}}
}
\value{
df.out A data frame of the events of interet from the
country, including geographic coordinates.
}
\description{
By default, this goes back to Jan 1 2000, but you can
change it. Inputs must be characters and EventRootCodes
must include leading zeros. Requires the \code{countrycode}
package to translate from country name to FIPS104. Assumes
you have GDELT in a dplyr tble in tables called "hist.db"
and "daily.db".  I have them in a SQLite database, but
dplyr will let you use whatever you want.  See Hadley
Wickham's github page.
}
\examples{
mex.protest <- subsetEventCountry("14", "Mexico", min.date==20000101)
}
\keyword{GDELT,}
\keyword{gdeltr}
\keyword{geographic,}

