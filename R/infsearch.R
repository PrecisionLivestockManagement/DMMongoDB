#' Retrieves infrastructure information from the DataMuster database
#'
#' This function allows infrastructure information to be retreived from the DataMuster database via the DataMuster website
#' @name  infsearch
#' @param property the name of the property to search the database
#' @param username a username to access the DataMuster database, contact Lauren O'Connor for database access
#' @param password a password to access the DataMuster database
#' @return a dataframe of infrastructure and associated information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


infsearch <- function(property=NULL, active=NULL, infstype=NULL, unitname=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(property)){
  property <- get_stations(username = username, password = password,
                           fields = c("_id", "longitude", "latitude", "reports", "PIC", "timezone", "stationname"))
  property <- property$Stationname}

  property <- paste(unlist(property), collapse = '", "' )

  if(is.null(active) || active == "FALSE"){
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)}else{
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}, "properties.datarecording":"%s"}', property, "TRUE")}

  lookfor <- sprintf('{"stationname":true, "properties.asset_id":true, "properties.Paddock":true, "properties.datarecording":true, "properties.type":true,
                       "properties.telemetry_out":true, "properties.lastsignal":true, "properties.filename":true, "properties.usenum":true, "properties.filename":true,
                       "properties.statdate":true, "properties.statEID":true, "properties.statweight":true, "_id":true}')

  infsinfo <- infrastructure$find(query = filterstation, fields = lookfor)

  if (nrow(infsinfo) == 0){infsinfo <- infrastructure$find(query = '{"stationname":"xxxxxx"}', fields = lookfor)}

  infsinfo$properties["stationname"] <- infsinfo$stationname
  infsinfo$properties["id"] <- infsinfo$`_id`

  infsinfo <- infsinfo$properties

  if(nrow(infsinfo) != 0){infsinfo$statdate <- as.Date(infsinfo$statdate, tz = "Australia/Brisbane")}

  if(is.null(infstype)){}else{
  infsinfo <- infsinfo%>%
              filter(type %in% infstype)}

  if(is.null(unitname)){}else{
    infsinfo <- infsinfo%>%
      filter(filename %in% unitname)}

  return(infsinfo)

}


