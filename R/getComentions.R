#' Given a subsetted dataframe from the Global Knowledge Graph, return a df with co-mentions, formatted for Gephi.
#' 
#' This should have an option for specificying co-mentioned counties, people, oranizations
#'
#'
#' @param gkg.df A subset of the Global Knowledge Graph \code{gkg.df}
#' @param type Data types to subset: "persons", "organizations", "countries", or "placenames". \code{gkg.df}
#'
#' @return co-mentions A data frame containing count information.
#'
#' @keywords GDELT
#'
#' @export
#' 
#' @examples
#' ieds <- gkg[grep("LANDMINE", gkg$THEMES),]
#' ieds.orgs <- getCo-mentions(ieds, type="organizations")


getComentions <- function(gkg.df, type) {
  if (type=="organizations" | type=="orgs"){
    orgs <- gkg.df$ORGANIZATIONS
    orgs <- strsplit(orgs, split=";")
    nMax <- max(sapply(orgs, length))
    orgs <- cbind(t(sapply(orgs, function(i) i[1:nMax])))
    orgs <- as.data.frame(orgs)
    pst <- function(x) {paste0("\'", x, "\'")}
    orgs <- as.data.frame(lapply(orgs[,1:ncol(orgs)], FUN= function(x) {sapply(x, FUN=pst)}))
    return(orgs)
  }
  
  if (type=="persons"){
    persons <- gkg.df$PERSONS
    persons <- strsplit(persons, split=";")
    nMax <- max(sapply(persons, length))
    persons <- cbind(t(sapply(persons, function(i) i[1:nMax])))
    persons <- as.data.frame(persons)
    pst <- function(x) {paste0("\'", x, "\'")}
    persons <- as.data.frame(lapply(persons[,1:ncol(persons)], FUN= function(x) {sapply(x, FUN=pst)}))
    return(persons)
  }
  if (type=="countries"){
    countries <- gkg.df$LOCATIONS
    countries <- strsplit(countries, split=";")
    nMax <- max(sapply(countries, length))
    countries <- cbind(t(sapply(countries, function(i) i[1:nMax])))
    countries <- countries[,3]
    countries.df <- data.frame(row.names=1:nrow(countries))
    for (i in 1:ncol(countries)) {
      tmp <- countries[,i]
      tmp1 <- strsplit(tmp, split="#")
      tmp2 <- sapply(tmp1, "[", 3)
      countries.df <- cbind(countries.df, tmp2)
    }
    cc <- function(x) {countrycode(x, "fips104", "country.name")}
    countries.df <- as.data.frame(lapply(countries.df[,1:ncol(countries.df)],FUN = function(x) {sapply(x,FUN=cc)}))
    pst <- function(x) {paste0("\'", x, "\'")}
    countries.df <- as.data.frame(lapply(countries.df[,1:ncol(countries.df)], FUN= function(x) {sapply(x, FUN=pst)}))
    return(countries.df)
  }
  
}
