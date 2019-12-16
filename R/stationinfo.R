#' Retreive property information from the DataMuster database
#'
#' This function allows property information to be retreived from the DataMuster database via the DataMuster website
#' @name  stationinfo
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a dataframe showing information associated with the property
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


stationinfo <- function(property=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  station <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)


  lookfor <- sprintf('{"name":true, "PIC":true, "timezone":true, "longitude":true, "latitude":true, "_id":true}')

  if (is.null(property)){
    propertyinfo <- station$find(query = '{}', fields=lookfor)}else{
    property <- paste(unlist(property), collapse = '", "' )
    filterstation <- sprintf('{"name":{"$in":["%s"]}}', property)
    propertyinfo <- station$find(query = filterstation, fields=lookfor)}

  propertyinfo <- propertyinfo %>%
    rename(Name = "name", Timezone = "timezone")
  return(propertyinfo)

}
