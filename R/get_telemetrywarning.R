#' Retrieve telemetry warnings from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve telemetry warnings from the TelemetryWarnings collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_telemetrywarning
#' @param property the property of the WOW with no telemetry signal
#' @param assetid the asset id of the WOW with no telemetry signal
#' @param lastsignal the last signal of the WOW with no telemetry signal
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers with the list of fields defined in the inputs and searched using the search terms
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_telemetrywarning <- function(property, assetid, lastsignal, resolved, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  tel <- mongo(collection = "TelemetryWarnings", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "')
  property <- sprintf('"property":{"$in":["%s"]},', property)

  assetid <- paste(unlist(assetid), collapse = '", "')
  assetid <- sprintf('"assetid":{"$in":["%s"]},', assetid)

  lastsignal <- paste(unlist(lastsignal), collapse = '", "')
  lastsignal <- sprintf('"lastsignal":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(lastsignal, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  resolved <- paste(unlist(resolved), collapse = '", "')
  resolved <- sprintf('"resolved":{"$in":["%s"]},', resolved)

  search <- paste0("{", property, assetid, lastsignal, resolved, "}")
  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

  data <- tel$find(query = search)

}
