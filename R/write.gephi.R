#' Wrapper for write.table for outputting from the GKG to Gephi
#' 
#' Specifically, it adds quotes to prevent extra splitting, removes row/col names, and saves with a semicolon separator.
#' Obvs., it's undirected.
#'
#' @param gkg.df A dataframe to export to gephi \code{gkg.df}
#' @param filename The name for the  \code{gkg.df}
#'
#' @return gkg.df A semicolon seperated file with quotes.
#'
#' @keywords GDELT
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

write.gephi <- function(gkg.df, filename) { 
pst <- function(x) {paste0("\'", x, "\'")}
gkg.df <- as.data.frame(lapply(gkg.df[,1:ncol(gkg.df)], FUN= function(x) {sapply(x, FUN=pst)}))
write.table(gkg.df, file=filename, sep=";", row.names=FALSE, col.names=FALSE)
}