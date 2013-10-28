#' Given a dataframe of "Counts" information from the GKG
#' 
#' The Global Knowledge Graph contains two elements, the "Counts" file, containing information on the numbers of people killed, affected, etc. every day by location.
#' The second file, the "Graph file", contains the associated themes, organizations, people, and locations.  Filtering, especially by themes, is very useful, but much of the useable information in the GKG is in the Counts file.  This function will return the Counts file, nicely formatted (it's \code{;} and \code{#} separated, which is a hassle), in no particular order.  It loses the date and theme information, though, which is the next room for improvement.
#'
#' @param gkg A subset dataframe of the GKG. \code{gkg}
#'
#' @return counts A data frame containing information from the \code{Counts} column. 
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' # Say we were interested in the number of people killed by mines/IEDs.
#' ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
#' ieds.counts <- GKGcounts(ieds)

GKGcounts <- function(gkg) {
  counts <- gkg$COUNTS
  counts <- strsplit(counts, split=";")
  counts <- unlist(counts)
  counts <- strsplit(counts, split="#")
  nMax <- max(sapply(counts, length))
  counts <- cbind(t(sapply(counts, function(i) i[1:nMax])))
  counts <- as.data.frame(counts)
  return(counts)
}