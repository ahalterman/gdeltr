\name{toner}
\alias{toner}
\title{Given a GKG subset, return the tones associated with each person/place/organization}
\usage{
  toner(df, type)
}
\arguments{
  \item{df}{A subset of the GKG, probably along one
  theme\code{df}}

  \item{type}{Return tones of organization, locations, or
  persons?}

  \item{summarize}{Should the mean for each unique entity
  be returned?  Caution: lots of alt. spellings
  \code{summarize}}
}
\value{
  tones A df with names/locations and tones (and counts if
  summarized).
}
\description{
  summarize will return the mean tone for each entity.
  This feature isn't done yet.
}
\examples{
> ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
> person.tone.ieds <- toner(ieds, type="persons")
> dim(person.tone.ieds)
[1] 4545    2
}
\keyword{GDELT,}
\keyword{gdeltr}

