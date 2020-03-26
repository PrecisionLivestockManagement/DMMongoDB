#' Retrieve ALMS use information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve ALMS use from the ALMSUse collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_almsuse
#' @param RFID a list of cattle RFID number/s
#' @param location the name of the property to search for
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones, default is Australia/Brisbane
#' @param fields a list of headers from the ALMSUse collection in the DataMuster MongoDB database to be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers with the list of fields defined in the inputs and searched using the search terms
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_almsuse <- function(RFID = NULL, location = NULL, start = NULL, end = NULL, timezone = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  #if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
                          RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(location)){}else{location <- paste(unlist(location), collapse = '", "' )
  location <- sprintf('"ALMS":{"$in":["%s"]},', location)}

  if(is.null(start)){}else{
    start <- sprintf('"Date":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}

  if(is.null(end)){}else{
    end <- sprintf('"Date":{"$lt":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end+1, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}


pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

almsuse <- mongo(collection = "ALMSUse", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, location, start, end, "}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format

data <- almsuse$find(query = search, fields = snappy)

if(nrow(data) == 0){
  dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}else{

      # Brings all data up to the same level

      # for(i in 1:ncol(data)){
      #   class <- class(data[,i])
      #   if(class == "data.frame"){
      #     data <- cbind(data, data[,i])
      #     data <- data[,-i]}
      # }

      # Formats any date columns with the correct timezone

      collist <- colnames(data)

       for(i in 1:length(collist)){
         if("POSIXt" %in% class(data[,i])){
           attributes(data[,i])$tzone <- timezone}}}

dataf <- data

}

