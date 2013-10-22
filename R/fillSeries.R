#' Fills in missing dates in a data frame of GDELT events for plotting or time series analysis
#' 
#'
#' @param df A GDELT dataframe.  \code{df}
#' @param begin.date The earliest date.  Defaults to Jan 1, 2000.  \code{begin.date}
#' @param end.date The last date.  Defaults to Sept 30, 2013.  \code{end.date}
#' @param date.column The name of the column containing dates.  Defaults to "SQLDATE" \code{date.column}
#' 
#'
#' @return df2 A 
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works
#' 

fillSeries <- function(df, begin.date="2000-01-01", end.date="2013-09-30", date.column="SQLDATE", extraclean=FALSE){
  daily <- as.data.frame(seq(from=as.Date(begin.date), to=as.Date(end.date), by="1 day"))
  names(daily) <- "Date"
  df <- merge(x=daily, y=df, by.x="Date", by.y=date.column, all.x=TRUE)
  if (extraclean==TRUE) {
  df[is.na(df$count),5] <- 0
  df <- df[,c(1:5)]
  names(df) <- c("Date", "ActionGeo_Lat", "ActionGeo_Long", "EventRootCode", "Count")
  return(df)
  }
  else {
  return(df)
  }
}