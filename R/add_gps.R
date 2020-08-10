#' Add GPS data to the DataMuster MongoDB database
#'
#' This function adds GPS data to the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_gps
#' @param RFID a list of cattle RFID number/s
#' @param DateTime a list of timestamps in POSIXct format
#' @param lat the latitude of a coordinate point
#' @param long the longitude of a coordinate point
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export


add_gps <- function(RFID, mtag, DateTime, roundedtime, lat, long, proximity, neighbour, username, password){


  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPSIoT <- mongo(collection = "AnitaGPS", db = "PLMResearch",
    url = pass,
    verbose = T)

  GPSdata <- sprintf('{"RFID":"%s", "Management":"%s", "timestamp":{"$date":"%s"}, "roundedtime":{"$date":"%s"},  "lat":%s, "long": %s ,
                     "proximity":"%s", "neighbour":"%s"}',
                     RFID, mtag, paste0(substr(DateTime,1,10),"T",substr(DateTime,12,19),"+1000"),
                     paste0(substr(roundedtime,1,10),"T",substr(roundedtime,12,19),"+1000"), lat, long,
                     proximity, neighbour)

  GPSIoT$insert(GPSdata)



}
