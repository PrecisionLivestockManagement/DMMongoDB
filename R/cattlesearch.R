#' Retrieve data on cattle from the DataMuster database
#'
#' This function provides a list of cattle RFID numbers with associated general information (category, breed, sex, property, paddock, database status). If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name cattlesearch
#' @param RFID a list of cattle RFID numbers to query
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers and associated information
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


cattlesearch <- function(RFID, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  RFID1 <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID1)

  lookfor <- sprintf('{"RFID":true, "stationname":true, "active":true, "properties.Management":true, "properties.breed":true,"properties.sex":true,"properties.category":true,"properties.Paddock":true, "properties.ALMS":true, "_id":false}')
  #lookfor <- sprintf('{"RFID":true, "stationname":true, "_id":false}')

  propertyinfo <- cattle$find(query = filterstation, fields=lookfor)
  propertyinfo$properties["RFID"] <- propertyinfo$RFID
  propertyinfo$properties["stationname"] <- propertyinfo$stationname
  propertyinfo$properties["active"] <- propertyinfo$active
  propertyinfo <- propertyinfo$properties

  miss <- RFID[!(RFID %in% propertyinfo$RFID)]

  if (length(miss) != 0){

    miss1 <- paste(unlist(miss), collapse = '", "' )
    filterstation1 <- sprintf('{"RFIDhist.ID":{"$in":["%s"]}}', miss1)
    RFIDsearch <- cattle$find(query = filterstation1, fields=lookfor)
    RFIDsearch$properties["RFID"] <- RFIDsearch$RFID
    RFIDsearch$properties["stationname"] <- RFIDsearch$stationname
    RFIDsearch$properties["active"] <- RFIDsearch$active
    RFIDsearch <- RFIDsearch$properties

    propertyinfo <- rbind(propertyinfo, RFIDsearch)

    print(paste0("The following RFID has superseded a RFID number in the original list ", RFIDsearch$RFID))
  }

  if (class(propertyinfo) != "NULL" && nrow(propertyinfo) >= 1){

  propertyinfo <- propertyinfo %>%
    select(RFID, Management, active, stationname, Paddock, ALMS, category, breed, sex)}

  return(propertyinfo)

}
