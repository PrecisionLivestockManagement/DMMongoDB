#' Add GPS data to the DataMuster MongoDB database
#'
#' This function adds GPS data to the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_gps
#' @param RFID a list of cattle RFID number/s
#' @param DateTime a list of timestamps in POSIXct format
#' @param lat the latitude of a coordinate point
#' @param long the longitude of a coordinate point
#' @param voltage the voltage of the device as a number or percentage
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export

#Just need to add a read.csv object as the data stream e.g.
add_wowraw <- function(data, username, password){


  library(mongolite)
  library(jsonlite)
  library(dplyr)

  names(data) <- "data"

  weights <- data%>%filter(data<5000)
  RFID <- data%>%filter(data>5000)
  RFIDlocation <- which(data$data %in% RFID$data, arr.ind=TRUE)

  print(RFIDlocation[1])

  print(weights)

  print(RFID)

  alldata <- toJSON(data$data)
  id <- toJSON(RFID$data)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  WoWData <- mongo(collection = "WoWData", db = "WoWRaw",
                   url = pass,
                   verbose = T)

  total <- sprintf('{"RawData":%s, "RFID":%s}', alldata, id)


  WoWData$insert(total)
}
