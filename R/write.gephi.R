#' Wrapper for write.table for outputting from the GKG to Gephi
#' 
#' Specifically, it adds quotes to prevent extra splitting, removes row/col names, and saves with a semicolon separator.
#' Obvs., it's undirected.
#' If it's a node list, the nodes MUST be in a column labeled "ID".
#'
#' @param gkg.df A dataframe to export to gephi \code{gkg.df}
#' @param filename The name for the file.  Call it .csv even though its semicolons \code{gkg.df}
#' @param type ragged or list?  List will generate an edge list rather than a ragged data frame.
#'
#' @return gkg.df A semicolon seperated file with quotes.
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

write.gephi <- function(gkg.df, filename, type) { 
pst <- function(x) {paste0("\'", x, "\'")}
if (type=="edge") {
gkg.df <- as.data.frame(lapply(gkg.df[,1:ncol(gkg.df)], FUN= function(x) {sapply(x, FUN=pst)}))
}
if (type=="node"){
  gkg.df$ID <- pst(gkg.df$ID)
}

write.table(gkg.df, file=filename, sep=";", row.names=FALSE, col.names=TRUE)
}