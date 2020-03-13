#' Retrieve cattle information via the DataMuster website app
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what fields should be returned via a list of fields.
#' @name get_dailywts
#' @param property property to search for
#' @param sex male or female
#' @param zoom whole property or paddock zoomchoice
#' @param paddock paddock on property
#' @param category the class of cattle either breeding or growing
#' @param alms TRUE or FALSE, if true filters the data for cattle currently allocated to an alms unit
#' @param timezone the timezone that applies to the cattle data
#' @param fields a list of MongoDB cattle collection headers that you want returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle with the list of fields defined in the inputs and searched using the search terms.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_dailywts <- function(RFID = NULL, location = NULL, start = NULL, end = NULL, minwt = NULL, timezone, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  #if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
                          RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(location)){}else{location <- paste(unlist(location), collapse = '", "' )
  location <- sprintf('"Location":{"$in":["%s"]},', location)}

  if(is.null(start)){}else{
    if(timezone == "Australia/Brisbane"){
    start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
      if(timezone == "America/Argentina/Buenos_Aires"){
        start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
    }

  if(is.null(end)){}else{
    if(timezone == "Australia/Brisbane"){
    end <- sprintf('"datetime":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
      if(timezone == "America/Argentina/Buenos_Aires"){
        end <- sprintf('"datetime":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
    }

  if(is.null(minwt)){}else{minwt <- sprintf('"Wt":{"$gte":%s},', minwt)}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

dailywts <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, location, start, end, minwt,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- dailywts$find(query = search, fields = snappy)

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
                 rename_all(recode, datetime = "Date", Wt = "Weight")
#                mutate_at(vars(ends_with("Date")), as.character, format = "%b %d %Y")%>%
#                mutate_at(vars(ends_with("Date")), funs(ifelse(. == "Jan 01 1970" | . == "Dec 31 1969", "", .)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(round(as.numeric(.), 0)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(ifelse(. == 0, as.character(""), as.character(.))))%>%
#                mutate_at(vars(starts_with("Hours")), funs(round(as.numeric(difftime(s, ., units = "hours")),0)))%>%
#                mutate_at(vars(starts_with("Hours")), funs(ifelse(. > 1000, NA, .)))%>%
#                select(RFID, Tag, Sex, Category, Paddock, everything())%>%
#                filter(RFID != "xxxxxx")
}

dataf

}

