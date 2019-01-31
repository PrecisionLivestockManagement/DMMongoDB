# Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#
# This is an example function named propsearch that provides a list of RFID numbers
# for a property from MongoDB. It asks for a username and password that are used to
# access the DataMuster MongoDB Atlas server
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username this is the unername to be able to access the MongDB and is enetered via a prompt
#' @param password this is the password to be able to acces the MongoDB
#' @return a dataframe that with a list of the RFID numbers


propsearch <- function(property){
  username = rstudioapi::showPrompt(title = "Username", message = "Username", default="")
  password =  rstudioapi::askForPassword("Enter Password")
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)


  filterstation <- sprintf('{"stationname":"%s"}', property)
  cattle$find(query = filterstation, fields='{"RFID":true, "_id":false}')


}
