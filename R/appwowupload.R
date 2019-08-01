#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function allows WoW data to be uploaded from the DataMuster website
#' @name appwowupload
#' @param RFID the RFID number as txt
#' @param datetime in correct format
#' @param weight in correct format
#' @param ALMS in correct format
#' @param username you will need to request access from Lauren O'Connor for a username to write data to the database
#' @param password you will need to request access from Lauren O'Connor for a username to write data to the database
#' @return uploads data in a dataframe to the DMMongoDB database
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @export


appwowupload <- function(RFID, datetime, weight, ALMS, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  wowdata <- mongo(collection = "WoWData", db = "DMIoT", url = pass, verbose = T)

  data <- sprintf('{"RFID":"%s", "datetime":{"$date":"%s"}, "Wt":%s, "Location": %s , "createdAt": %s }', RFID, paste0(substr(datetime,1,10),"T",substr(datetime,12,19),"+1000"), weight, ALMS, system.time())

  wowdata$insert(data)

}
