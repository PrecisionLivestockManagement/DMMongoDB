#' Remove animals from the autodrafter in the DataMuster MongoDB database.
#'
#' This function removes individual or groups of cattle in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name remove_autodraft
#' @param RFID a list of the previous cattle RFID number/s
#' @param active whether the autodrafter is still active
#' @param alms the new ALMS ID
#' @param direction the direction of the autodrafter
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

remove_autodraft <- function(RFID, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  autodraft <- mongo(collection = "AutoDraft", db = "DataMuster", url = pass, verbose = T)

  # Checking RFID is in the correct format
  checkRFID <- paste(unlist(RFID), collapse = '", "' )
  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the previous RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  ##### Update Cattle collection #####
  for(i in 1:length(RFID)){
    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
    cattfinder <- cattle$find(query = RFIDS,
                          fields = '{"_id":false, "RFID":true, "properties.AUTODRAFT":true, "properties.AUTODRAFTasset_id":true, "properties.AUTODRAFT_dir":true}')
    updater <- sprintf('{"$set":{"properties.AUTODRAFT":"%s", "properties.AUTODRAFTasset_id":"%s", "properties.AUTODRAFT_dir":"%s"}}', "FALSE", "xxxxxx", "xxxxxx")
    cattle$update(RFIDS, updater)

  ##### Remove from AutoDraft collection #####
    autodraft$remove(query = RFIDS, just_one = FALSE)

  }

  }


