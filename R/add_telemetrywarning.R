#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function adds data for WOWs with no telemetry signals in the last 3 hours to the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_telemetrywarning
#' @param property the property of the WOW with no telemetry signal
#' @param assetid the asset id of the WOW with no telemetry signal
#' @param filename the filename of the WOW with no telemetry signal
#' @param paddock the paddock of the WOW with no telemetry signal
#' @param lastsignal the last signal of the WOW with no telemetry signal
#' @param emailtime the time that the initial alert email is sent
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


add_telemetrywarning <- function(property, assetid, filename, paddock, lastsignal, emailtime, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  tel <- mongo(collection = "TelemetryWarnings", db = "DataMuster", url = pass, verbose = T)

  emailtime <- Sys.time()
  notes <- ""

  data <- sprintf('{"property":"%s", "assetid":"%s", "filename":"%s", "paddock":"%s", "lastsignal":{"$date":"%s"}, "emailtime":{"$date":"%s"}, "notes":"%s"}',
                    property, assetid, filename, paddock, strftime(as.POSIXct(paste0(lastsignal, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"),
                    strftime(as.POSIXct(paste0(emailtime, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), notes)

  tel$insert(data)

}
