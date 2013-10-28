#' Given a GKG subset, return the tones associated with each person/place/organization
#' 
#' summarize will return the mean tone for each entity.  This feature isn't done yet.
#'
#' @param df A subset of the GKG, probably along one theme\code{df}
#' @param type Return tones of organization, locations, or persons?
#' @param summarize Should the mean for each unique entity be returned?  Caution: lots of alt. spellings \code{summarize}
#'
#' @return tones A df with names/locations and tones (and counts if summarized).
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' > ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
#' > person.tone.ieds <- toner(ieds, type="persons")
#' > dim(person.tone.ieds)
#' [1] 4545    2


toner <- function(df, type){
  if (!"TONE" %in% names(df)) stop("No column named 'TONE' in input data frame")
  if (rnow(df)==0) stop("Input data frame has 0 rows")
  
  if (type=="Person" | "person" | "persons" | "PERSONS") {
  persontone <- data.frame(stringsAsFactors=FALSE)
  if (!"PERSONS" %in% names(df)) stop("No column named 'PERSONS' in input data frame")
  for (i in 1:nrow(df)) {
    line <- df[i,]
    persons <- line$PERSONS
    tone <- line$TONE
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
  #  if (type=="locations" | "LOCATIONS") {
  #   if (!"LOCATIONS" %in% names(df)) stop("No column named 'LOCATIONS' in input data frame")
  # }

  # if (summarize=TRUE){
  # take the df, group_by column 1, column 2=mean(column2)
  # return
  # }
}
