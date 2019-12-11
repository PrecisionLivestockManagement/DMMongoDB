#' Update RFID tag number via the DataMuster website app
#'
#' This function pulls in daily weights for individual or groups of cattle for specified periods. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name appupdateRFID
#' @param RFID this is a list of the previous cattle RFID numbers
#' @param MTag this is a list of the cattle management tag numbers
#' @param property this is the name of the station
#' @param newRFID this is a list of the new cattle RFID numbers
#' @param date provide the date that the new RFID tag was applied, this has to be in date format. Default is today's date.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

appupdateRFID <- function(RFID, MTag, property, newRFID, date, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  # Checks that the RFID numbers are in the correct format for the database

  if("TRUE" %in% (nchar(as.character(newRFID))!= 16)) {
    stop(paste0("One or more of the new RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  # Checks that the new RFID numbers are not already registered in the database

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checknewRFID)
  check <- cattle$count(query = filtercattle)

  if (check != 0) {

    stop("One or more of the new RFID numbers are already registered in the database. Please check that the RFID numbers are correct and try again")}


  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxx"){
      IDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
        IDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

          banger <- cattle$find(query = IDS, fields='{"RFIDhist.date":true, "_id":false}')
          arrpos <- length(banger$RFIDhist$date[[1]])
          IDIlast <- sprintf('{"$set":{"RFID":"%s"}}', newRFID[i])
          IDI <- sprintf('{"$set":{"RFIDhist.date.%s":{"$date":"%s"}, "RFIDhist.ID.%s":"%s"}}', arrpos, paste0(date,"T","00:00:00","+1000"), arrpos, newRFID[i])

      cattle$update(IDS, IDI) # Have to do this one first before RFID changes
      cattle$update(IDS, IDIlast)

  }

  }


