#' Standardize names in a data frame.
#' 
#' This will be very much a function in progress.  The focus is on Syrian names right now.
#' Credit to http://susanejohnston.wordpress.com/ for find-and-replace code.
#'
#' @param data A vector of names \code{gkg.df}
#'
#' @return newvec A vector with cleaned names
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' Some R code here


nameFixer <- function(namevector) {
  if (is.factor(namevector)){
    namevector <- as.character(namevector)
    }
    oldvalue <- nameFixer_data[,1]
    newvalue <- nameFixer_data[,2]
   
    newvec <- namevector    
    for (i in unique(oldvalue)) newvec[namevector == i] <- newvalue[oldvalue == i]
    return(newvec)
  }