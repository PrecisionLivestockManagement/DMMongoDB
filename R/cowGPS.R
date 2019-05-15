#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function allows GPS data to be added for an indivdual animal and can be used to export server data into the DataMuster MongoDB data storage and provide live updates
#' @name cowGPS
#' @param RFID the RFID number as txt
#' @param DateTime in correct format
#' @param lat in correct format as a number
#' @param long in correct format as a number
#' @param username you will need to request access from Lauren O'Connor for a username to write data to the database
#' @param password you will need to request access from Lauren O'Connor for a username to write data to the database
#' @return a dataframe with a list of the RFID numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @export


cowGPS <- function(RFID, DateTime, lat, long, username, password){


  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPSIoT <- mongo(collection = "GPS", db = "DMIoT",
    url = pass,
    verbose = T)

  GPSdata <- sprintf('{"RFID":"%s", "DateTime":{"$date":"%s"}, "lat":%s, "long": %s }', RFID, paste0(substr(DateTime,1,10),"T",substr(DateTime,12,19),"+1000"), lat, long)

  GPSIoT$insert(GPSdata)



}
