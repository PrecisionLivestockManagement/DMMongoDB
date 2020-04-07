#' Update RFID using MTag in the DataMuster MongoDB database.
#'
#' This function updates individual or groups of RFID numbers using MTag in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_RFIDMTag
#' @param MTag a list of cattle management tag number/s
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param date the date that the new RFID tag was applied, in date format. Default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the RFID tags have been successfully updated
#' @author Anita Chang \email{a.chang@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


update_RFIDMTag <- function(MTag, RFID, property, date, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

# Check that the property is registered in the database ---------------------------------------------------------------------

    filterstation <- sprintf('{"stationname":"%s"}', property)
    station <- stations$count(query = filterstation)

    if(station == 0) {
      stop('Could not find matching property. Please check spelling and ensure the property is registered in the database.')}


# Find cows in the database

    if (!(is.null(MTag))){
      checkcows <- paste(unlist(MTag), collapse = '", "' )

      filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcows)
      cows <- cattle$find(query = filtercattle, fields = '{"properties.Management":true, "stationname":true, "_id":false}')

      cows <- cows%>%filter(stationname == property)

      if (nrow(cows) < length(MTag)) {

        problemcowtags <- as.character(MTag[!(MTag %in% cows$properties$Management)])

        if (length(problemcowtags) != 0){ #Indicates they are not in the database

        stop(paste0("The following MTag numbers cannot be found in the database. Please check that the MTag numbers are correct and try again: "), problemcowtags)}
    }}



  #  Update animal information --------------------------

      for (i in 1:length(MTag)){


        IDS <- sprintf('{"stationname":"%s","properties.Management":"%s"}', property, MTag[i])

        banger <- cattle$find(query= IDS, fields='{"RFIDhist.date":true, "_id":false}')
        arrpos <- length(banger$RFIDhist$date[[1]])
        RFIDIlast <- sprintf('{"$set":{"RFID":"%s"}}', RFID[i])
        RFIDI <- sprintf('{"$set":{"RFIDhist.date.%s":{"$date":"%s"}, "RFIDhist.ID.%s":"%s"}}', arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, RFID[i])

        cattle$update(IDS, RFIDI) # Has to update RFIDI first otherwise won't update RFIDhist
        cattle$update(IDS, RFIDIlast)

      }
}
