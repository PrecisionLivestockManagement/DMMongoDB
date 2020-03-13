#' Add GPS data to the DataMuster MongoDB database
#'
#' This function allows GPS data to be added to the DataMuster MongoDB database
#' @name add_gps
#' @param RFID a list of cattle RFID numbers
#' @param DateTime a list of timestamps in POSIXct format
#' @param lat the latitude of a coordinate point
#' @param long the longitude of a coordinate point
#' @param voltage the voltage of the device as a number or percentage
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully added
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @export


add_gps <- function(RFID,
                    DateTime,
                    lat,
                    long,
                    voltage,
                    username,
                    password){


  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPSIoT <- mongo(collection = "GPS", db = "DMIoT",
    url = pass,
    verbose = T)

  GPSdata <- sprintf('{"RFID":"%s", "DateTime":{"$date":"%s"}, "lat":%s, "long": %s , "voltage": %s }', RFID, paste0(substr(DateTime,1,10),"T",substr(DateTime,12,19),"+1000"), lat, long, voltage)

  GPSIoT$insert(GPSdata)



}
