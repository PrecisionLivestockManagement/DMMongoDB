#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' for a property from MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name propsearch
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username this is the unername to be able to access the MongDB and is enetered via a prompt
#' @param password this is the password to be able to acces the MongoDB
#' @return a dataframe that with a list of the RFID numbers
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


propsearch <- function(property, paddock=NULL){


  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)


  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)
  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "properties.Paddock":true, "_id":false}')
  propertyinfo <- cattle$find(query = filterstation, fields=lookfor)

  return(propertyinfo)

}
