#' Graph changes in tone over time, given a GKG subset.
#' 
#' This takes a GKG dataframe and organizations/themes/people of interest and returns the tone of daily news coverage of that entity. Right now it only does themes.
#' 
#'
#' @param df A subset of the Global Knowledge Graph, probably a country \code{gkg.df}
#' @param object A vector of your entities of interest (persons, themes, or organizations.)
#' @param type persons, themes, or organizations? Only works for "theme" now.
#' @param location A location, potentially more specific than the data frame subset.
#' @param overlay Should the lines be plotted on the same graph or separate?
#' @param returndata If true, returns the raw data and does not plot anything
#' @param span How much smoothing on the loess curve?
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
#' toneTrend(mex, c("CRIME_CARTELS", "SECURITY_SERVICES", "KILL"), type="theme", location="Mexico")


toneTrend <- function(df, objects, type, location, overlay=TRUE, span=0.3, returndata=FALSE){
  require(ggplot2)
  theme.counts <- data.frame()
  for(i in 1:length(objects)){
    # loop through the themes vector, return # per day of each.
    object.i <- objects[i]
    if(type=="theme" | type=="THEMES"){
      tmp <- df[grep(object.i, df$THEMES),]
      tones <- strsplit(as.character(tmp$TONE), ",")
      tmp$tone <- as.numeric(sapply(tones, "[", 1))
      tmp$type <- tolower(gsub("_", " ", object.i))
      theme.counts <- rbind(theme.counts, tmp)
    }
  }
  theme.counts <- theme.counts[,c("DATE", "tone", "type")]
  # just the cols we need, condense by day
  theme.counts$DATE <- gDate(theme.counts$DATE)
  theme.counts <- as.data.frame(summarise(group_by(tbl_df(theme.counts), DATE, type), Number=n(), Tone=mean(tone)))
  #maxheight <- max(theme.counts$Number) * 1.05
  if(returndata==TRUE){
    return(theme.counts)
    stop()
  }
  if(overlay==TRUE){
    # all on the same graph
    return(ggplot(data=theme.counts, aes(x=DATE, y=Tone, color=type)) + geom_point(size=2, alpha=0.7) + geom_smooth(method="loess", span=span, se=FALSE, size=1) + ylab("Tone")  + theme_bw())
  }
  if(overlay==FALSE){
    # on different graphs
    return(ggplot(data=theme.counts, aes(x=DATE, y=Tone, type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=span, se=FALSE, size=1) + facet_wrap(~ type, ncol=1) + theme_bw())
  }
}