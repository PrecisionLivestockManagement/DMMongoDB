#' Retrieve static weight data from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve static weight data from the StaticWts collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_staticwts
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param minwt the minimum weight to be returned
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones
#' @param fields a list of headers from the StaticWts collection in the DataMuster MongoDB database to be returned. If not specified, the RFID, static weight date, static weight, and property name will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated static weight records
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_staticwts <- function(RFID = NULL, property = NULL, start = NULL, end = NULL, minwt = NULL, cattle_id = NULL, timezone = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
                          RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(property)){}else{property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  if(is.null(start)){}else{
    if(timezone == "Australia/Brisbane"){
    start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
      if(timezone == "America/Argentina/Buenos_Aires"){
        start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
    }

  if(is.null(end)){}else{
    if(timezone == "Australia/Brisbane"){
    end <- sprintf('"Date":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
      if(timezone == "America/Argentina/Buenos_Aires"){
        end <- sprintf('"Date":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
    }

  if(is.null(minwt)){}else{minwt <- sprintf('"Wt":{"$gte":%s},', minwt)}

  if(is.null(cattle_id)){}else{cattle_id <- paste(unlist(cattle_id), collapse = '", "' )
  cattle_id <- sprintf('"cattle_id":{"$in":["%s"]},', cattle_id)}

  if(is.null(fields)){
    fields = c("RFID", "Date", "Weight", "stationname")}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

staticwts <- mongo(collection = "StaticWts", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, property, start, end, minwt, cattle_id,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- staticwts$find(query = search, fields = snappy)

# If no data is returned an empty dataframe is created

if(nrow(data) == 0){
  dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}else{

collist <- colnames(data)

 for(i in 1:length(collist)){
   if("POSIXt" %in% class(data[,i])){
     attributes(data[,i])$tzone <- timezone}}

 # dataf <- data%>%
 #          rename_all(recode, datetime = "Date", Wt = "Weight")

 dataf <- data

    }

dataf

}

