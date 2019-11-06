#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of ALMS units with use history for a property from MongoDB. Inputs need to be a list of one or more property names
#' @name infsearch
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the data numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


infsearch <- function(property=NULL, active=NULL, infstype=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(property)){
  property <- stationinfo(username = username, password = password)
  property <- property$Name}

  property <- paste(unlist(property), collapse = '", "' )

  if(is.null(active) || active == "FALSE"){
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)}else{
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}, "properties.datarecording":"%s"}', property, "TRUE")}

  lookfor <- sprintf('{"stationname":true, "properties.asset_id":true, "properties.Paddock":true, "properties.datarecording":true, "properties.type":true,
                       "properties.telemetry_out":true, "properties.lastsignal":true, "properties.usenum":true, "properties.filename":true,
                       "properties.statdate":true, "properties.statEID":true, "properties.statweight":true, "_id":false}')

  infsinfo <- infrastructure$find(query = filterstation, fields = lookfor)

  if (nrow(infsinfo) == 0){infsinfo <- infrastructure$find(query = '{"stationname":"xxxxxx"}', fields = lookfor)}

  infsinfo$properties["stationname"] <- infsinfo$stationname

  infsinfo <- infsinfo$properties

  if(nrow(infsinfo) != 0){infsinfo$statdate <- as.Date(infsinfo$statdate, tz = "Australia/Brisbane")}

  if(is.null(infstype)){}else{
  infsinfo <- infsinfo%>%
              filter(type %in% infstype)}

  return(infsinfo)

}


