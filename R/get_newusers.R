#' Retrieve cattle information via the DataMuster website app
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what fields should be returned via a list of fields.
#' @name get_newusers
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


get_newusers <- function(start = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(start)){}else{
  start <- as.POSIXct(start)

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

