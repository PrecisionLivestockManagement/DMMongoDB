#' Retrieve ALMS historical information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve historical ALMS information from the ALMSHistory collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_almshistory
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param ALMS a list of ALMS asset_id's to search for
#' @param currentALMS the current ALMS unit allocation, TRUE or FALSE
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones
#' @param fields a list of headers from the DailyWts collection in the DataMuster MongoDB database to be returned. If not specified, the RFID, stationname, ALMS, dateON, and dateOFF will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated ALMS allocation statistics
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_almshistory <- function(RFID = NULL, MTag = NULL, property = NULL, ALMS = NULL, currentALMS = NULL, timezone = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
                          RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(MTag)){}else{MTag <- paste(unlist(MTag), collapse = '", "' )
  MTag <- sprintf('"Management":{"$in":["%s"]},', MTag)}

  if(is.null(property)){}else{property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  if(is.null(ALMS)){}else{ALMS <- paste(unlist(ALMS), collapse = '", "' )
  ALMS <- sprintf('"ALMS":{"$in":["%s"]},', ALMS)}

  if(is.null(currentALMS)){}else{currentALMS <- paste(unlist(currentALMS), collapse = '", "' )
  currentALMS <- sprintf('"currentALMS":{"$in":["%s"]},', currentALMS)}

  if(is.null(fields)){
    fields = c("RFID", "stationname", "ALMS", "dateON", "dateOFF")}

  # if(is.null(start)){}else{
  #   if(timezone == "Australia/Brisbane"){
  #   start <- sprintf('"dataON":{"$lte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
  #     if(timezone == "America/Argentina/Buenos_Aires"){
  #       start <- sprintf('"dataON":{"$lte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
  #   }
  #
  # if(is.null(end)){}else{
  #   if(timezone == "Australia/Brisbane"){
  #   end <- sprintf('"dataOFF":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}else{
  #     if(timezone == "America/Argentina/Buenos_Aires"){
  #       end <- sprintf('"dataOFF":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}}
  # }

  # if(!is.null(end) && end == format(Sys.Date(), tz = timezone)){
  #   today <- sprintf('"dateOFF":{"$exists":{"%s}},', "FALSE")}else{today <- NULL}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

almshistory <- mongo(collection = "ALMSHistory", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, MTag, property, ALMS, currentALMS,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":true}', te)

#Query database and format for website display

data <- almshistory$find(query = search, fields = snappy)

# If no data is returned an empty dataframe is created

if(nrow(data) == 0){
  dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}else{

collist <- colnames(data)

 for(i in 1:length(collist)){
   if("POSIXt" %in% class(data[,i])){
     attributes(data[,i])$tzone <- timezone}}

 if(is.null(data$dateOFF)){
 dataf <- data%>%
          mutate(dateOFF = NA)}else{

  dataf <- data
          }
}

dataf

}

