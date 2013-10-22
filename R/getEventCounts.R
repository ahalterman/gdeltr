#' Get event counts per country-month from GDELT
#' 
#' By default, this goes back to Jan 1 2000, but you can change it.
#' Inputs must be characters.
#' Requires the \code{countrycode} package to translate from country name to FIPS104.
#' Assumes you have GDELT in a dplyr tble in tables called "hist.db" and "daily.db".  I have them in a SQLite database, but dplyr will let you use whatever you want.  See Hadley Wickham's github page.
#'
#'It pains me to say, but I think I'll need to pre-build the output df to include all possible EventCodes to allow merging with other countries.
#'
#' @param country.name A normal English country name (character) \code{country.name}
#' @param eventtype What event code resolution?  Options: "code", "base", "root" \code{country.name}
#' @param min.date Furthest date back you want (numeric).  \code{min.date}
#'
#' @return df.out A data frame counts per event per month in the country 
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' mex.protest <- getEventCounts("Mexico", eventtype="root", min.date==20000101)


getEventCounts <- function(countryname, eventtype="root", min.date=20000101){
  fips.code <- countrycode(countryname, "country.name", "fips104")
  iso.code <- countrycode(countryname, "country.name", "iso3c")
  if (eventttype=="root") {
    df <- select(hist.db, SQLDATE, MonthYear, EventRootCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventRootCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventRootCode)
    df[is.na(df)] <- 0
    return(df)
  }
  if (eventttype=="base") {
    df <- select(hist.db, SQLDATE, MonthYear, EventBaseCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventBaseCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventBaseCode)
    df[is.na(df)] <- 0
    return(df)
  }  
  if (eventttype=="code") {
    df <- select(hist.db, SQLDATE, MonthYear, EventCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventCode)
    df[is.na(df)] <- 0
    return(df)
  }  
}