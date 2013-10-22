#' Get event counts per country-month from GDELT
#' 
#' By default, this goes back to Jan 1 2000, but you can change it.
#' Inputs must be characters.
#' Requires the \code{countrycode} package to translate from country name to FIPS104.
#' Assumes you have GDELT in a dplyr tble in tables called "hist.db" and "daily.db".  I have them in a SQLite database, but dplyr will let you use whatever you want.  See Hadley Wickham's github page.
#'
#' Still need to build the scaffolding for EventBaseCode (who uses that though?).  QuadClass will be fine without a scaffold.
#'
#'
#' @param country.name A normal English country name (character) \code{country.name}
#' @param eventtype What event code resolution?  Options: "code", "base", "root" \code{country.name}
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
    
    code.scaffold <- data.frame(matrix(NA, nrow = 0, ncol = 20))
    names(code.scaffold) <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20")                            
    df <- merge(root.scaffold, df, all.x=TRUE)
    df2 <- select(daily.db, SQLDATE, MonthYear, EventRootCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df2(df2)
    df2 <- group_by(df2, EventRootCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventRootCode)
    df2[is.na(df2)] <- 0
    df <- merge(df, df2, all.x=TRUE)
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
    df2 <- select(daily.db, SQLDATE, MonthYear, EventBaseCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df2(df2)
    df2 <- group_by(df2, EventBaseCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventBaseCode)
    df2[is.na(df2)] <- 0
    df <- rbind(df, df2)
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
    code.scaffold <- data.frame(matrix(NA, nrow = 0, ncol = 311))
    names(code.scaffold) <- c("01","010","011","012","013","014","015","016","017","018","019","02","020","021","0211","0212","0213","0214","022","023","0231","0232","0233","0234","024","0241","0242","0243","0244","025","0251","0252","0253","0254","0255","0256","026","027","028","03","030","031","0311","0312","0313","0314","032","033","0331","0332","0333","0334","034","0341","0342","0343","0344","035","0351","0352","0353","0354","0355","0356","036","037","038","039","04","040","041","042","043","044","045","046","05","050","051","052","053","054","055","056","057","06","060","061","062","063","064","07","070","071","072","073","074","075","08","080","081","0811","0812","0813","0814","082","083","0831","0832","0833","0834","084","0841","0842","085","086","0861","0862","0863","087","0871","0872","0873","0874","09","090","091","092","093","094","10","100","101","1011","1012","1013","1014","102","103","1031","1032","1033","1034","104","1041","1042","1043","1044","105","1051","1052","1053","1054","1055","1056","106","107","108","11","110","111","112","1121","1122","1123","1124","1125","113","114","115","116","12","120","121","1211","1212","122","1221","1222","1223","1224","123","1231","1232","1233","1234","124","1241","1242","1243","1244","1245","1246","125","126","127","128","129","13","130","131","1311","1312","1313","132","1321","1322","1323","1324","133","134","135","136","137","138","1381","1382","1383","1384","1385","139","14","140","141","1411","1412","1413","1414","142","1421","1422","1423","1424","143","1431","1432","1433","1434","144","1441","1442","1443","1444","145","1451","1452","1453","1454","15","150","151","152","153","154","16","160","161","162","1621","1622","1623","163","164","165","166","1661","1662","1663","17","170","171","1711","1712","172","1721","1722","1723","1724","173","174","175","18","180","181","182","1821","1822","1823","183","1831","1832","1833","184","185","186","19","190","191","192","193","194","195","196","20","200","201","202","203","204","2041","2042")  
    
    df2 <- select(daily.db, SQLDATE, MonthYear, EventCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
    df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
    df2 <- as.data.frame(df2)
    df2 <- tbl_df2(df2)
    df2 <- group_by(df2, EventCode, MonthYear)
    df2 <- summarise(df2, count=n())
    df2 <- as.data.frame(df2)
    df2 <- dcast(df2, MonthYear ~ EventCode)
    df2[is.na(df2)] <- 0
    df <- rbind(df, df2)
    df <- merge(df, df2, all.x=TRUE)
    return(df)
  }  
  if (eventttype=="quad") {
  df <- select(hist.db, SQLDATE, MonthYear, QuadClass, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
  df <- filter(df, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
  df <- as.data.frame(df)
  df <- tbl_df(df)
  df <- group_by(df, QuadClass, MonthYear)
  df <- summarise(df, count=n())
  df <- as.data.frame(df)
  df <- dcast(df, MonthYear ~ QuadClass)
  df[is.na(df)] <- 0
  
  df2 <- select(daily.db, SQLDATE, MonthYear, QuadClass, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode)
  df2 <- filter(df2, SQLDATE >= min.date, ActionGeo_CountryCode==fips.code, Actor1CountryCode==iso.code, Actor2CountryCode==iso.code)
  df2 <- as.data.frame(df2)
  df2 <- tbl_df2(df2)
  df2 <- group_by(df2, QuadClass, MonthYear)
  df2 <- summarise(df2, count=n())
  df2 <- as.data.frame(df2)
  df2 <- dcast(df2, MonthYear ~ QuadClass)
  df2[is.na(df2)] <- 0
  df <- rbind(df, df2)
  return(df)
}  
}