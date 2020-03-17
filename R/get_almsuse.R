#' Retrieve cattle information via the DataMuster website app
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what fields should be returned via a list of fields.
#' @name get_almsuse
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

