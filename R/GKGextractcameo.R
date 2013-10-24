#' Extract CAMEO events from GKG
#'
#' From a GKG subset dataframe, return a dataframe of all linked CAMEO event IDs.
#' If \code{justvector=TRUE}, the fuction will return only a vector of linked CAMEO event IDs rather than the complete dataframe of all matching events. 
#'
#' @param df A dataframe of GKG namesets
#' @param justvector Return vector of ID numbers instead of actual dataframe?
#'
#' @return gdelt.df A vector of all linked CAMEO event IDs 
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' cameos.events <- GKGextractcameo(mexico.cartels)

GKGextractcameo <- function(df, justvector=FALSE) {
 raw <- df$CAMEOEVENTIDS 
 cameoeventids <- unlist(strsplit(raw, split=","))
 if (justvector==TRUE){
 return(cameoeventids)
 }
 if (justvector==FALSE){
   hist <- as.data.frame(filter(hist.db, GLOBALEVENTID==cameoeventids))
daily <- as.data.frame(filter(daily.db, GLOBALEVENTID==cameoeventids))
     gdelt.df <- rbind(hist, daily)
return(gdelt.df)
 }
}