#' Given a subsetted dataframe from the Global Knowledge Graph, return the info in the "Counts" field as a data frame.
#' 
#' This will only give you the info in the counts field, and in no particular order.
#' Next steps: getting the date and themes to come with it.
#'
#' @param gkg.df \code{gkg.df}
#'
#' @return counts A data frame containing count information.
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

GKGcounts <- function(gkg.df) {
  counts <- gkg.df$COUNTS
  counts <- strsplit(counts, split=";")
  counts <- unlist(counts)
  counts <- strsplit(counts, split="#")
  nMax <- max(sapply(counts, length))
  counts <- cbind(t(sapply(counts, function(i) i[1:nMax])))
  counts <- as.data.frame(counts)
  return(counts)
}