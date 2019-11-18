#' Retrieve data on cattle ALMS use from the DataMuster database
#'
#' This function provides a list of cattle usage, by date, of ALMS units for a property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name ALMSuse
#' @param property the name of the property to query
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe of the cattle RFID numbers and ALMS visit count per day (0 = no visit, 1 = visit)
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


ALMSuse <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  almsuse <- mongo(collection = "ALMSUse", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  filterdata <- sprintf('{"Property":{"$in":["%s"]}}', property)

  data <- almsuse$find(query = filterdata, fields = '{"_id":false}')

  if (nrow(data) != 0){
  data <- data%>%
    mutate(Date = as.Date(Date, tz = "Australia/Brisbane"))}


  return(data)
}
