#' Get event counts per country-month from GDELT
#' 
#' By default, this goes back to Jan 1 2000, but you can change it.
#' Inputs must be characters.
#' Requires the \code{countrycode} package to translate from country name to FIPS104.
#' Assumes you have GDELT in a dplyr tble in tables called "hist.db" and "daily.db".  I have them in a SQLite database, but dplyr will let you use whatever you want.  See Hadley Wickham's github page.
#'
#' Reverted to a prev. version.  No pre-build scaffolding to get all of the columns.
#'
#'
#' @param country.name A normal English country name (character) \code{country.name}
#' @param eventtype What event code resolution?  Options: "code", "base", "root", "quad". \code{eventtype}
#' @param min.date Furthest date back you want (numeric).  \code{min.date}
#'
#' @return df A data frame counts per event per month in the country 
#'
#' @keywords GDELT, gdeltr
#'
#' @export
#' 
#' @examples
#' mex.protest <- getEventCounts("Mexico", eventtype="root", min.date==20000101)


getEventCounts <- function(countryname, eventtype="root", min.date=20000101){
  require(countrycode)
  require(reshape2)
  require(dplyr)
  require(RSQLite)
  require(RSQLite.extfuns)
  
  fips.code <- countrycode(countryname, "country.name", "fips104")
  iso.code <- countrycode(countryname, "country.name", "iso3c")
  
  if (eventtype=="root") {
    df <- select(hist.db, SQLDATE, MonthYear, EventRootCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventRootCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventRootCode)

    df2 <- select(daily.db, SQLDATE, MonthYear, EventRootCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df(df2)
    df2 <- group_by(df2, EventRootCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventRootCode)
    df <- merge(df, df2, all=TRUE)
    df[is.na(df)] <- 0
    return(df)
  }
  if (eventtype=="base") {
    df <- select(hist.db, SQLDATE, MonthYear, EventBaseCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventBaseCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventBaseCode)

    df2 <- select(daily.db, SQLDATE, MonthYear, EventBaseCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df(df2)
    df2 <- group_by(df2, EventBaseCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventBaseCode)
    df <- merge(df, df2)
    df[is.na(df)] <- 0
    return(df)
  }  
  if (eventtype=="code") {
    df <- select(hist.db, SQLDATE, MonthYear, EventCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df <- as.data.frame(df)
    df <- tbl_df(df)
    df <- group_by(df, EventCode, MonthYear)
    df <- summarise(df, count=n())
    df <- as.data.frame(df)
    df <- dcast(df, MonthYear ~ EventCode)
  
    df2 <- select(daily.db, SQLDATE, MonthYear, EventCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df(df2)
    df2 <- group_by(df2, EventCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventCode)
    df <- merge(df, df2, all=TRUE)
    df[is.na(df)] <- 0
    return(df)
  }  
  if (eventtype=="quad") {
  df <- select(hist.db, SQLDATE, MonthYear, QuadClass, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
  df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
  df <- as.data.frame(df)
  df <- tbl_df(df)
  df <- group_by(df, QuadClass, MonthYear)
  df <- summarise(df, count=n())
  df <- as.data.frame(df)
  df <- dcast(df, MonthYear ~ QuadClass)
  
  df2 <- select(daily.db, SQLDATE, MonthYear, QuadClass, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
  df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
  df2 <- as.data.frame(df2)
  df2 <- tbl_df(df2)
  df2 <- group_by(df2, QuadClass, MonthYear)
  df2 <- summarise(df2, count=n())
  df2 <- as.data.frame(df2)
  df2 <- dcast(df2, MonthYear ~ QuadClass)
  df <- merge(df, df2, all=TRUE)
  df[is.na(df)] <- 0
  return(df)
}  
}