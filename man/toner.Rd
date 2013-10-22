\name{toner}
\alias{toner}
\title{Given a GKG subset, return the tones associated with each person/place/organization}
\usage{
  toner(df)
}
\arguments{
  \item{df}{A subset of the GKG, probably along one
  theme\code{df}}

  \item{summarize}{Should the mean for each unique entitty
  be returned?  Caution: lots of alt. spellings
  \code{summarize}}
}
\value{
  tones A df with names/locations and tones (and counts if
  summarized).
}
\description{
  summarize will return the mean tone for each entity.
  eventually, include type Return tone for persons,
  countries, locations, or organizations? \code{type}
}
\examples{
> ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
> person.tone.ieds <- toner(ieds)
> dim(person.tone.ieds)
[1] 4545    2
}
\keyword{GDELT,}
\keyword{gdeltr}

