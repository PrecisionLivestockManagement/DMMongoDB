#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_foetalagedata
#' @param RFID a list of cattle RFID number/s
#' @param date the date of scanning in date format
#' @param foetalage the foetal age at scanning
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


add_foetalagedata <- function(RFID, date, foetalage, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    calvingdata <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)

# Find cows in the database

      cows <- get_cattle(RFID = RFID, username = username, password = password)

      if (nrow(cows) < length(RFID)) {

        problemcows <- as.character(RFID[!(RFID %in% cows$RFID)])

        if (length(problemcows) != 0){ #Indicates they are not in the database

          stop(paste0("The following RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again: "), problemcows)}
      }

    #  Update CalvingData collection --------------------------

    template <- calvingdata$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')

    for (i in 1:length(RFID)){

      cow <- cows[cows$RFID == RFID[i],]
      temp <- template

      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

      if(foetalage[i] != 0){

      temp$RFID <- RFID[i]
      temp$cow_id <- cow$`_id`
      temp$stationname <- cow$stationname
      temp$foetalagedate <- as.POSIXct(date[i])
      temp$foetalage <- foetalage[i]
      temp$estcalvingdate <- as.POSIXct(date[i] + (41 - foetalage[i]) * 7)
      temp$season <- paste(strftime(temp$foetalagedate, format = "%Y"), "/", substr(as.numeric(strftime(temp$foetalagedate, format = "%Y"))+1, 3, 4), sep = "")
      temp$Management <- cow$Management

      calvingdata$insert(temp)

        RFIDI <- sprintf('{"$set":{"properties.foetalagedate":{"$date":"%s"}, "properties.foetalage":%s, "properties.estcalvingdate":{"$date":"%s"}}}',
                         paste0(substr(date[i],1,10),"T","00:00:00","+1000"), foetalage[i], paste0(substr(temp$estcalvingdate,1,10),"T","00:00:00","+1000"))
        cattle$update(RFIDS, RFIDI)}else{

        RFIDI <- sprintf('{"$set":{"properties.foetalagedate":{"$date":"%s"}, "properties.foetalage":%s, "properties.estcalvingdate":{"$date":"%s"}}}',
                         paste0(substr(date[i],1,10),"T","00:00:00","+1000"), foetalage[i], paste0("1970-01-01","T","14:00:00","+0000"))
        cattle$update(RFIDS, RFIDI)
      }
      }

}






