#' Ignore a load ramp record
#'
#' This function updates a record in the MongoDB database. You can only access this function if you have read and write permission
#' @name loadrampignore
#' @param RFID this is a list of cattle RFID numbers
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return message to say the record has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


loadrampignore <- function(RFID, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    loadramps <- mongo(collection = "LoadRamps", db = "DMIoT", url = pass, verbose = T)

    for (i in 1:length(RFID)){
      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
      RFIDI <- sprintf('{"$set":{"actioned":"%s"}}', "1")
      loadramps$update(RFIDS, RFIDI)}
}
