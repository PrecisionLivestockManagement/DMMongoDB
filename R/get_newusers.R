#' Retrieve new user information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve new user information from the NewUser collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_newusers
#' @param start a start date to be returned in date format, default is “2014-09-01”
#' @param fields a list of headers from the NewUser collection in the DataMuster MongoDB database to be returned. If not specified, the login email and date of creation will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated user information
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_newusers <- function(start = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(fields)){
    fields = c("loginemail", "createdAt")}

  if(is.null(start)){}else{

  start <- as.POSIXct(paste0(start, "00:00:00"))

  trumpper <- strftime(start, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT")

  start <- sprintf('"createdAt":{"$gte":{"$date":"%s"}},', trumpper)}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

newusers <- mongo(collection = "NewUsers", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", start,"}")

  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- newusers$find(query = search, fields = snappy)

dataf <- data

if(nrow(dataf) != 0){
 dataf <- dataf%>%
          filter(grepl("@", loginemail))
}

dataf

}

