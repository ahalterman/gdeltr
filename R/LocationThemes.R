#' In a GKG subset, how many times are given themes mentioned in conjunction with given locations?
#' 
#' This takes a GKG dataframe, a list of themes, and a list of countries and plots the distribution of mentions per country.
#' 
#'
#' @param df A subset of the Global Knowledge Graph including more than one country and one theme \code{df}
#' @param themes A vector of themes from GKG. \code{themes}
#' @param countries A vector of countries in country name form \code{countries}
#'
#' @return theme.counts A data frame containing counts per theme per country, suitable for faceted barplotting.
#'
#' @keywords GDELT, gdeltr
#' 
#'
#' @export
#' 
#' @examples
#' latin.protests <- LocationThemes(protests, themes=c("SLUMS", "ECON", "NEW_CONSTRUCTION", "VIOLENT_UNREST", "PUBLIC_TRANSPORT", "EDUCATION"), countries=c("Brazil", "Argentina", "Venezuela", "Colombia", "Uruguay", "Paraguay", "Bolivia", "Ecuador", "Peru", "Chile", "Mexico", "Honduras"))
#' ggplot(latin.protests, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=5) + theme_bw() + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + labs(x=NULL)



LocationThemes <- function(df, themes, countries){
  # "df" will be the overarching big theme, like protests, for the whole world
  theme.counts <- data.frame()
  for(i in 1:length(themes)){
    type.i <- themes[i]
    locations <- df[grep(type.i, df$THEMES), "LOCATIONS"]
    locations <- as.character(unlist(strsplit(locations, ";")))
    locations <- strsplit(as.character(locations), "#")
    locations <- sapply(locations, "[", 3)
    locations <- as.data.frame(table(locations))
    locations$type <- tolower(gsub("_", " ", type.i))
    theme.counts <- rbind(theme.counts, locations)
  }
  names(theme.counts) <- c("Country", "Count", "Theme") 
  countrylist <- countrycode(countries, "country.name", "fips104")
  theme.counts <- theme.counts[theme.counts$Country %in% countrylist,]
  #print("Second loop done")
  #print(head(theme.counts))
  theme.counts$Country <- countrycode(theme.counts$Country, "fips104", "country.name")
  countrytotals <- as.character(unlist(strsplit(df$LOCATIONS, ";")))
  countrytotals <- strsplit(as.character(countrytotals), "#")
  countrytotals <- sapply(countrytotals, "[", 3)
  countrytotals <- as.data.frame(table(countrytotals))
  names(countrytotals) <- c("Country", "Total")
  countrytotals$Country <- countrycode(countrytotals$Country, "fips104", "country.name")
  theme.counts <- merge(theme.counts, countrytotals, by="Country")
  theme.counts$Percent <- theme.counts$Count / theme.counts$Total
  return(theme.counts)
}

