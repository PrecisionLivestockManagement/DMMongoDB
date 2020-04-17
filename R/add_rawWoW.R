#' Add Raw WoW data to the WoWRaw MongoDB database
#'
#' This function adds raw WoW data to the WoWRaw MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_wowraw
#' @param data is a single file of a column of weights including the RFID read that is inserted between the weights when it is read
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @export

# Just need to add a read.csv object as the data stream e.g. data <- read.csv('20200116_121648_Kraatz_WoW_1.txt')
# The plan is to include some summary statistics as the data is uploaded. For example the RFID location finds where the
# the RFID tag occurs in the weight stream. Greg is going to send me details of his algorithm so we can run his weight
# calculation at the same time that we add the data. The function needs to have a wrapper to read multiple
# csv files.
add_wowraw <- function(data, username, password){


  library(mongolite)
  library(jsonlite)
  library(dplyr)

  names(data) <- "data"

  weights <- data%>%filter(data<5000)
  RFID <- data%>%filter(data>5000)
  RFIDlocation <- which(data$data %in% RFID$data, arr.ind=TRUE)

 # print(RFIDlocation[1])

 # print(weights)

 # print(RFID)

  alldata <- toJSON(data$data)
  id <- toJSON(RFID$data)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  WoWData <- mongo(collection = "WoWData", db = "WoWRaw",
                   url = pass,
                   verbose = T)

  total <- sprintf('{"RawData":%s, "RFID":%s}', alldata, id)


  WoWData$insert(total)
}
