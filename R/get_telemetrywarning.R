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


get_telemetrywarning <- function(property = NULL, assetid = NULL, lastsignal = NULL, resolved = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  tel <- mongo(collection = "TelemetryWarnings", db = "DataMuster", url = pass, verbose = T)

  if(is.null(property)){} else {
    property <- paste(unlist(property), collapse = '", "')
    property <- sprintf('"property":{"$in":["%s"]},', property)
  }

  if(is.null(assetid)){} else {
    assetid <- paste(unlist(assetid), collapse = '", "')
    assetid <- sprintf('"assetid":{"$in":["%s"]},', assetid)
  }

  if(is.null(lastsignal)){} else {
    lastsignal <- paste0(strftime(lastsignal, format = "%Y-%m-%d %H:%M"), ":00")
  }

  if(is.null(resolved)){} else {
    resolved <- paste(unlist(resolved), collapse = '", "')
    resolved <- sprintf('"resolved":{"$in":["%s"]},', resolved)
  }

  # Set up find query
  search <- paste0("{", property, assetid, resolved, "}")
  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

  # Set up find fields
  # fields = c("property", "assetid", "resolved")
  # snif <- sprintf('"%s":true', fields)
  # te <- paste0(snif, collapse = ", ")
  # snappy <- sprintf('{%s, "_id":false}', te)

  # Query database
  data <- tel$find(query = search)
  data <- data %>%
    dplyr::filter(lastsignal %in% lastsignal)

  return(data)
}
