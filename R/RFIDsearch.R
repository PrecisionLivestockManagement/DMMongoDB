#' Search for cattle using RFID in the DataMuster MongoDB database.
#'
#' This function retrieves RFID from the DataMuster MongoDB database.
#' @name RFIDsearch
#' @param RFID a list of cattle RFID number/s
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


RFIDsearch <- function(RFID, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  RFIDlist <- paste(unlist(RFID), collapse = '", "' )

  returnfields <- sprintf('{"RFID":true, "_id":false}')

  searchRFID <- sprintf('{"RFID":{"$in":["%s"]}}', RFIDlist)

  matched <- cattle$find(query = searchRFID, fields = returnfields)

  missing <- RFID[!(RFID %in% matched$RFID)]

  if (length(missing) == 0){}else{

    missingRFIDlist <- paste(unlist(missing), collapse = '", "' )

    searchRFIDhist <- sprintf('{"RFIDhist.ID":{"$in":["%s"]}}', missingRFIDlist)

    found <- cattle$find(query = searchRFIDhist, fields = returnfields)

    matched <- rbind(matched, found)
  }

  return(matched)

}
