#' Subset GDELT by a given EventRootCode and country name, returning lat/long for each event.
#' 
#' By default, this goes back to Jan 1 2000, but you can change it.
#' Inputs must be characters and EventRootCodes must include leading zeros.
#' Requires the \code{countrycode} package to translate from country name to FIPS104.
#' Assumes you have GDELT in a dplyr tble in tables called "hist.db" and "daily.db".  I have them in a SQLite database, but dplyr will let you use whatever you want.  See Hadley Wickham's github page.
#'
#' @param event.root.code One of the 20 EventRootCodes in CAMEO/GDELT, including leading zero \code{event.root.code}
#' @param country.name A normal English country name \code{country.name}
#' @param min.date Furthest date back you want.  \code{min.date}
#'
#' @return df.out A data frame of the events of interet from the country, including geographic coordinates. 
#'
#' @keywords GDELT, geographic, gdeltr
#'
#' @export
#' 
#' @examples
#' mex.protest <- subsetEventCountry("14", "Mexico", min.date==20000101)


subsetEventCountry <- function(event.root.code, country.name, min.date=20000101){
  country.code <- countrycode(country.name, "country.name", "fips104")
  df <- select(hist.db, SQLDATE, EventRootCode, ActionGeo_CountryCode, ActionGeo_Lat, ActionGeo_Long)
  df <- filter(df, SQLDATE >= min.date, EventRootCode==event.root.code, ActionGeo_CountryCode==country.code)
  df <- group_by(df, SQLDATE, ActionGeo_Lat, ActionGeo_Long, EventRootCode)
  df <- summarise(df, count=n())
  df <- as.data.frame(df)
  
  df.daily <- select(daily.db, SQLDATE, EventRootCode, ActionGeo_CountryCode, ActionGeo_Lat, ActionGeo_Long)
  df.daily <- filter(df.daily, EventRootCode==event.root.code, ActionGeo_CountryCode==country.code)
  df.daily <- group_by(df.daily, SQLDATE, EventRootCode, ActionGeo_Lat, ActionGeo_Long)
  df.daily <- summarise(df.daily, count=n())
  df.daily <- as.data.frame(df.daily)
  
  df.out <- rbind(df, df.daily)
  
  df.out$SQLDATE <- gDate(df.out$SQLDATE)
  
  return(df.out)
}