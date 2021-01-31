#' Add an autodrafter to the DataMuster MongoDB database.
#'
#' This function updates the ALMS unit in the Infrastructure and Cattle collections and adds a new document to the AutoDraft collection in the DataMuster MongoDB database. You can only access this function if you have read and write permissions.
#' @name add_autodraft
#' @param RFID the RFIDs of the cattle on the autodrafter
#' @param mtag the management tags of the cattle on the autodrafter
#' @param property the name of the property
#' @param paddock the name of the paddock
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password, contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the infrastructure has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


add_autodraft <- function(RFID, mtag, property, paddock, alms, direction, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    inf <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    autodraft <- mongo(collection = "AutoDraft", db = "DataMuster", url = pass, verbose = T)

    # Check that the RFID numbers are in the correct format and exist in the database
    if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
      stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}
    checkcows <- paste(unlist(RFID), collapse = '", "' )
    filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
    check <- cattle$count(query = filtercattle)
    if (check != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

    ##### Update Infrastructure collection #####
    for(i in 1:length(alms)){
      RFIDS <- sprintf('{"properties.asset_id":"%s"}', alms[i])
      updater <- sprintf('{"$set":{"properties.autodraft":"%s"}}', "TRUE")
      inf$update(RFIDS, updater)
    }

    ##### Update the Cattle collection #####
    for(i in 1:length(RFID)){
      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
      updater <- sprintf('{"$set":{"properties.AUTODRAFT":"%s", "properties.AUTODRAFTasset_id":"%s", "properties.AUTODRAFT_dir":"%s"}}',
                         "TRUE", alms[i], direction[i])
      cattle$update(RFIDS, updater)
    }

    ##### Add to the AutoDraft collection #####
    for(i in 1:length(RFID)){
      template <- data.frame(RFID = RFID[i], mtag = mtag[i], property = property, paddock = paddock[i],
                             asset_id = alms[i], direction = direction[i])
      autodraft$insert(template)
    }
}
