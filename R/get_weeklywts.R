#' Retrieve weekly weight information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve weekly weight information from the WeeklyWts collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_weeklywts
#' @param RFID a list of cattle RFID number/s
#' @param start a start date and time to be returned in datetime format, default is “2014-09-01 00:00:00”
#' @param end an end date and time to be returned in datetime format, default is today’s date and time
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones, default is Australia/Brisbane
#' @param fields a list of headers from the WeeklyWts collection in the DataMuster MongoDB database to be returned. If not specified, the RFID, date, weekly weight, sd of the weights, number of weights, and property name will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated weekly weight statistics
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_weeklywts <- function(RFID = NULL, start = NULL, end = NULL, timezone = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
                          RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(start)){}else{
    start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}

  if(is.null(end)){}else{
    end <- sprintf('"Date":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}

  if(is.null(fields)){
    fields = c("RFID", "Date", "avweight", "sdweights", "numweights", "Location")}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

weeklywts <- mongo(collection = "WeeklyWts", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, start, end,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- weeklywts$find(query = search, fields = snappy)

dataf <- data

collist <- colnames(dataf)

if(nrow(dataf) != 0){
 for(i in 1:length(collist)){
   if("POSIXt" %in% class(dataf[,i])){
     attributes(dataf[,i])$tzone <- timezone}}
}

# s <- Sys.time()
# attr(s,"tzone") <- timezone

if(nrow(dataf) != 0){
 dataf <- dataf%>%
#                 rename_all(recode, datetime = "Date", Wt = "Weight")
                 mutate_at(vars(ends_with("Date")), as.Date, tz = "Australia/Brisbane")#%>%
#                mutate_at(vars(ends_with("Date")), funs(ifelse(. == "Jan 01 1970" | . == "Dec 31 1969", "", .)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(round(as.numeric(.), 0)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(ifelse(. == 0, as.character(""), as.character(.))))%>%
#                mutate_at(vars(starts_with("Hours")), funs(round(as.numeric(difftime(s, ., units = "hours")),0)))%>%
#                mutate_at(vars(starts_with("Hours")), funs(ifelse(. > 1000, NA, .)))%>%
#                select(RFID, Tag, Sex, Category, Paddock, everything())%>%
#                filter(RFID != "xxxxxx")
#}

dataf

}
}

