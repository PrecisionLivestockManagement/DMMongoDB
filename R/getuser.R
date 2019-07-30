#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' for a property from MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name getuser
#' @param email the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe showing the information associated with the property used to search
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


getuser <- function(email, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  users <- mongo(collection = "Users", db = "DataMuster", url = pass, verbose = T)

  lookfor <- sprintf('{"loginemail":true, "accesslevel":true, "writeaccess":true, "stations":true, "_id":false}')
  filterusers <- sprintf('{"loginemail":{"$in":["%s"]}}', email)

  userinfo <- users$find(query = filterusers, fields=lookfor)

  userinfo <- userinfo %>%
    select(loginemail, accesslevel, writeaccess, stations)

  return(userinfo)

}
