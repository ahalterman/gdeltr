\name{LocationThemes}
\alias{LocationThemes}
\title{In a GKG subset, how many times are given themes mentioned in conjunction with given locations?}
\usage{
LocationThemes(df, themes, countries)
}
\arguments{
  \item{df}{A subset of the Global Knowledge Graph
  including more than one country and one theme \code{df}}

  \item{themes}{A vector of themes from GKG. \code{themes}}

  \item{countries}{A vector of countries in country name
  form \code{countries}}
}
\value{
theme.counts A data frame containing counts per theme per
country, suitable for faceted barplotting.
}
\description{
This takes a GKG dataframe, a list of themes, and a list of
countries and plots the distribution of mentions per
country.
}
\examples{
latin.protests <- LocationThemes(protests, themes=c("SLUMS", "ECON", "NEW_CONSTRUCTION", "VIOLENT_UNREST", "PUBLIC_TRANSPORT", "EDUCATION"), countries=c("Brazil", "Argentina", "Venezuela", "Colombia", "Uruguay", "Paraguay", "Bolivia", "Ecuador", "Peru", "Chile", "Mexico", "Honduras"))
ggplot(latin.protests, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=5) + theme_bw() + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + labs(x=NULL)
}
\keyword{GDELT,}
\keyword{gdeltr}

