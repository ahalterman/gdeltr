#' Convert a ragged data frame into an edgelist
#' 
#' Because GKG's persons, organizations, etc. fields countain varying numbers of elements, converting them to a data frame will produce a ragged data frame (rows with different numbers of columns, albeit padded with NAs.
#' Gephi can import ragged data frames, thought not if you plan to include node attributes.  This function will take a ragged data frame and return a edgelist data frame (2 columns, lots of rows).
#' 
#' Right now, even with the apply setup (instead of the awful earlier for-loop) it's still really, really slow.  
#'
#'@details Some namesets contain hundreds of names.  To increase speed at the loss of some connections, you can limit the number of columns that are included.  By default, this is set to 30.
#'
#' @param df A subset of the GKG \code{df}
#' @param max.connections How many columns to include? Set a number or "all". Default is 30.
#'
#' @return edgelist A data frame with two columns containing the two nodes defining  each edge.
#'
#' @keywords GDELT, gdeltr
#'
#'@details 
#' Speet test with 1,000 x 30: \n
#' No compilation, omit NAs all at once at the end: 83.417 \n
#' With compilation, omit NAs all all at once at the end: 77.874 \n
#' No compilation, omit NAs every row: 69.892 \n
#' With compilation, omit NAs every row: 70.164 \n
#' [these weren't very scientific since I ran them once each and did other stuff in the background]
#'
#' @export
#' 
#' @examples
#' corruption<- gkg[grep("CORRUPTION", gkg$THEMES),]
#' corruption <- GKGcomentions(corruption, type="persons")
#' corruption.edgelist <- GKGedgelist(corruption, max.connections=40)
#' 

# is it faster to omit nas while it's running or after?

GKGedgelist <- function(df, max.connections=30) {
  # trim if needed
  if (max.connections!="all"){
    df <- df[,1:max.connections]
  }
  # the apply part of the function.  "combn" provides all the unique combos of x in length n=2. t() transposes.
  split.fun <- function(x){
    #  x <- x[!is.na(x)]
    x <- t(combn(x, 2))
    x <- x[!is.na(x[,1]),]
    x <- x[!is.na(x[,2]),]
    return(x)
  }
  # do the call over the length of the df, bind each result.  Remove NA's.  "complete.cases" wasn't working for me.
  edgelist <- do.call("rbind", lapply(df, split.fun))
}
