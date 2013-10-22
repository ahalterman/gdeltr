#' Given a vector of 8 digit dates (yyyymmdd), returns a date obj in "yyyy-mm-dd"
#' 
#' This comes up a lot working with GDELT.  
#' Make sure you only pass it a vector, not the whole data frame!
#'
#' @param date.vector A vector of the SQLDATE column from GDELT, in form yyyymmdd \code{date.vector}
#'
#' @return newdate A vector of class date with "yyyy-mm-dd" format  \code{newdate}
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works


gDate <- function(date.vector) {
  date.vector <- as.character(date.vector)
  x <- substr(date.vector, 1, 4)
  y <- substr(date.vector, 5, 6)
  z <- substr(date.vector, 7, 8)
  date.vector <- paste(x,y,z,sep="-")
  newdate <- as.Date(date.vector, format="%Y-%m-%d")
  return(newdate)
}