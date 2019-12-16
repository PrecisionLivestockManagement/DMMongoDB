#' Retrieves cattle ALMS useage information from the DataMuster database
#'
#' This function allows cattle ALMS use data to be retreived from the DataMuster database via the DataMuster website app
#' @name  ALMSuse
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle and a daily visit indicator, 0 = not recorded and 1 = recorded
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


ALMSuse <- function(property, start = NULL, username = NULL, password = NULL){

  days <- as.numeric(difftime(Sys.Date(), as.Date(start), unit = "days"))

  seconds <- days * 60 * 60 * 24

  Time <- Sys.time() - seconds

  trumpper <- strftime(Time, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT")

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  almsuse <- mongo(collection = "ALMSUse", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
if (is.null(start)){filterdata <- sprintf('{"Property":{"$in":["%s"]}', property)} else {
  filterdata <- sprintf('{"Property":{"$in":["%s"]},"Date": { "$gte" : { "$date" : "%s" }}}', property, trumpper)}

  data <- almsuse$find(query = filterdata, fields = '{"_id":false}')

  if (nrow(data) != 0){
  data <- data%>%
    mutate(Date = as.Date(Date, tz = "Australia/Brisbane"))}



  return(data)
}
