#' Update RFID tag number in the DataMuster MongoDB database.
#'
#' This function updates the RFID number for individual or groups of cattle in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_RFID
#' @param RFID a list of the previous cattle RFID number/s
#' @param newRFID a list of the new cattle RFID number/s
#' @param date the date that the new RFID tag was applied in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

update_RFID <- function(RFID, newRFID, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}

  checkRFID <- paste(unlist(RFID), collapse = '", "' )
  checknewRFID <- paste(unlist(newRFID), collapse = '", "' )

  # Checks that the RFID numbers are in the correct format for the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
      stop(paste0("One or more of the previous RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  if("TRUE" %in% (nchar(as.character(newRFID))!= 16)) {
    stop(paste0("One or more of the new RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}


  # Checks that the old RFID numbers exist in the database

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkRFID)
  check <- cattle$count(query = filtercattle)

  if (check != length(RFID)) {

    stop("One or more of the previous RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}


  # Checks that the new RFID numbers are not already registered in the database

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checknewRFID)
  check <- cattle$count(query = filtercattle)

  if (check != 0) {

    stop("One or more of the new RFID numbers are already registered in the database. Please check that the RFID numbers are correct and try again")}



  for (i in 1:length(RFID)){

   RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

          banger <- cattle$find(query = RFIDS, fields='{"RFIDhist.date":true, "_id":false}')
          arrpos <- length(banger$RFIDhist$date[[1]])
          RFIDIlast <- sprintf('{"$set":{"RFID":"%s"}}', newRFID[i])
          RFIDI <- sprintf('{"$set":{"RFIDhist.date.%s":{"$date":"%s"}, "RFIDhist.ID.%s":"%s"}}', arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, newRFID[i])

      cattle$update(RFIDS, RFIDI) # Has to update RFIDI first otherwise won't update RFIDhist
      cattle$update(RFIDS, RFIDIlast)

  }

  }


