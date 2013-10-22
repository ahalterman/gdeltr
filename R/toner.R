#' Given a GKG subset, return the tones associated with each person/place/organization
#' 
#' summarize will return the mean tone for each entity.
#' eventually, include type Return tone for persons, countries, locations, or organizations? \code{type}
#'
#' @param df A subset of the GKG, probably along one theme\code{df}
#' @param summarize Should the mean for each unique entitty be returned?  Caution: lots of alt. spellings \code{summarize}
#'
#' @return tones A df with names/locations and tones (and counts if summarized).
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' > ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
#' > person.tone.ieds <- toner(ieds)
#' > dim(person.tone.ieds)
#' [1] 4545    2


toner <- function(df){
  persontone <- data.frame(stringsAsFactors=FALSE)
  for (i in 1:nrow(df)) {
    line <- df[i,]
    persons <- line[,5]
    tone <- line[,7]
    tone <- unlist(strsplit(tone, ","))
    tone <- as.numeric(tone[1])
    persons <- as.character(unlist(strsplit(persons, ";")))
    ptone.tmp <- cbind(persons, rep(tone, length(persons)))
    persontone <- rbind(persontone, ptone.tmp)
  }
  persontone$persons <- as.character(persontone$persons)
  persontone$persons <- nameFixer(persontone$persons)
  persontone$V2 <- as.numeric(as.character(persontone$V2))
  persontone <- as.data.frame(summarise(group_by(tbl_df(persontone), persons), count=n(), meantone=mean(V2)))
  return(persontone)
}
