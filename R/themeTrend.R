#' Graph changes in themes over time, given a GKG subset.
#' 
#' This takes a GKG dataframe and a list of themes and plots the mentions of the themes over time.
#' 
#'
#' @param df A subset of the Global Knowledge Graph, probably a country \code{gkg.df}
#' @param themes A vector of themes from GKG.
#' @param location A location, potentially more specific than the data frame subset.
#' @param overlay Should the lines be plotted on the same graph or separate?
#' @param returndata If true, returns the raw data and does not plot anything
#'
#' @return theme.counts A data frame containing number of events per day per theme.
#'
#' @keywords GDELT, gdeltr
#' 
#'
#' @export
#' 
#' @examples
#' gkg <- read.csv("gkg.csv")
#' mex <- gkg[grep("Mexico", gkg$LOCATIONS),]
#' themeTrends(mex, c("CRIME_CARTELS", "SECURITY_SERVICES", "KILL"), location="Mexico")


themeTrend <- function(df, themes, location, overlay=TRUE, returndata=FALSE){
  require(gdeltr)
  require(ggplot2)
  # df should preferably just be COUNTS, THEMES, DATE for the region (or theme?) you're interested in.
  # location should be a FIPS104 or a city name
  theme.counts <- data.frame()
  for(i in 1:length(themes)){
    # loop through the themes vector, return # per day of each.
    type.i <- themes[i]
    tmp <- df[grep(type.i, df$THEMES),]
    tmp$Number <- sapply(tmp$COUNTS, function(x) length(grep(location, unlist(strsplit(x, ";")))))
    tmp$type <- type.i
    theme.counts <- rbind(theme.counts, tmp)
  }
  theme.counts <- theme.counts[,c("DATE", "Number", "type")]
  # just the cols we need, condense by day
  theme.counts <- as.data.frame(summarise(group_by(tbl_df(theme.counts), DATE, type), Number=sum(Number)))
  theme.counts$DATE <- gDate(theme.counts$DATE)
  maxheight <- max(theme.counts$Number) + 10
 
  if(returndata==TRUE){
    return(theme.counts)
    stop()
  }
  
  if(overlay==TRUE){
    # all on the same graph
    return(ggplot(data=theme.counts, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + ylim(0, maxheight) + ylab("Count")  + theme_bw())
  }
  
  if(overlay==FALSE){
    # on different graphs
    return(ggplot(data=theme.counts, aes(x=DATE, y=Number, type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + facet_wrap(~ type, ncol=1) + theme_bw())
  }
}
