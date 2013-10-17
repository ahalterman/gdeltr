#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works
#' 

fillSeries <- function(df){
  daily <- as.data.frame(seq(from=as.Date("2000-01-01"), to=as.Date("2013-09-28"), by="1 day"))
  names(daily) <- "Date"
  df2 <- merge(x=daily, y=df, by.x="Date", by.y="SQLDATE", all.x=TRUE)
  df2[is.na(df2$count),5] <- 0
  df2 <- df2[,c(1:5)]
  names(df2) <- c("Date", "ActionGeo_Lat", "ActionGeo_Long", "EventRootCode", "Count")
  return(df2)
}