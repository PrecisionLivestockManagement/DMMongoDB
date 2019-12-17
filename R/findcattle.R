#' Retrieve cattle information
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what values should be returned via a list of fields. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name findcattle
#' @param property property to search for
#' @param gender male or female
#' @param paddock paddock on property
#' @param status the class of cattle either breeding or growing
#' @param values a list of MongoDB cattle collection headers that you want returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle with the list of fields defined in the inputs and searched using the search terms.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


findcattle <- function(property = NULL, gender = NULL, paddock = NULL, status = NULL, values = NULL, username = NULL, password = NULL){



  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(property)){} else {property <- sprintf('"stationname":"%s",', property)}
  if(is.null(gender)){} else {gender <- sprintf('"properties.sex":"%s",', gender)}
  if(is.null(paddock)){} else {paddock <- sprintf('"properties.Paddock":"%s",', paddock)}
  if(is.null(status)){} else {status <- sprintf('"properties.category":"%s",', status)}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


cattlesearch <-paste0("{", property, gender, paddock, status,"}")
if(nchar(cattlesearch)==2){}else{
cattlesearch <- substr(cattlesearch, 1 , nchar(cattlesearch)-2)
cattlesearch <- paste0(cattlesearch, "}")}

if(is.null(values)){
cattle$find(query = cattlesearch, fields = '{"RFID":true, "stationname":true, "properties.Paddock":true, "_id":false}')
}else{
  snif <- sprintf('"%s":true', values)
  te <- paste0(snif, collapse = ", ")

  snappy <- sprintf('{%s, "_id":false}', te)

  print(cattlesearch)
  print(snappy)


  cattle$find(query = cattlesearch, fields = snappy)
}
}

