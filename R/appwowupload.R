#' Upload DataMuster WoW data to the DataMuster database
#'
#' This function allows DataMuster WoW data to be uploaded via the DataMuster website
#' @name appwowupload
#' @param RFID a list of the cattle RFID numbers
#' @param datetime a list of timestamps in POSIXct format
#' @param weight a list of cattle weights
#' @param ALMS a list of ALMS identification codes as character entries
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully uploaded
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


appwowupload <- function(RFID, datetime, weight, ALMS, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  wowdata <- mongo(collection = "appwow", db = "DMIoT", url = pass, verbose = T)

  RFID[is.na(RFID)] <- ""

  data <- sprintf('{"RFID":"%s", "datetime":{"$date":"%s"}, "Wt":%s, "Location": "%s" , "createdAt":{"$date":"%s"}}', RFID, paste0(substr(datetime,1,10),"T",substr(datetime,12,19),"+1000"), weight, ALMS, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  wowdata$insert(data)

}
