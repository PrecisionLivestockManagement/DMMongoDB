#' Reactivate cattle on a station in the DataMuster MongoDB database.
#'
#' This function reactivates individual or groups of cattle that have previously been removed from a station on the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name reactivatecattle
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param paddock the name of the paddock to search for
#' @param date the date that the animal left the station in date format, default is today's date
#' @param replacevalues the new RFID of the animal
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the cattle has been successfully reactivated and that the RFID tag number has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


reactivatecattle <- function(RFID, MTag, property, paddock, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}
  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(paddock) == 1){paddock <- rep(paddock, length = length(RFID))}

  # Check that the RFID numbers are in the correct format and exist in the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  #checkcows <- paste(unlist(RFID), collapse = '", "' )

  cows <- get_cattle(RFID = RFID, MTag = MTag, property = "xxxxxx", username = username, password = password,
                     fields = c("RFID", "properties.Management", "active",
                                "stationname", "properties.Paddock", "properties.ALMS",
                                "properties.category", "properties.breed", "properties.sex"))
  # cows <- cattlesearch(RFID, username = username, password = password)

  if (nrow(cows) != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  stationinfo <- get_stations(property, username = username, password = password,
                              fields = c("_id", "longitude", "latitude", "reports", "PIC", "timezone", "stationname"))

  for (i in 1:length(cows$RFID)){

    if (cows$active[i] == FALSE){

      if (RFID[i] != "xxx xxxxxxxxxxxx"){

        RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{

          RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', "xxxxxx", MTag[i])}

    RFIDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "properties.exitDate":{"$date":"%s"}}}',
                       property, stationinfo$`_id`, "TRUE", "xxxxxx", paste0("1970-01-01","T","00:00:00","+1000"))

    cattle$update(RFIDS, RFIDI)

    update_paddock(RFID = cows$RFID[i], MTag = cows$Management[i], property = property, paddock = paddock[i], date = date[i], username = username, password = password)

    #banger <- cattle$find(query= RFIDS, fields='{"pdkhist.dateIN":true, "_id":false}')
    #arrpos <- length(banger$pdkhist$dateIN[[1]])

    #temppad <- pads[which(pads$paddname == paddock[i]),]

    #RFIDIlast <- sprintf('{"$set":{"properties.PaddockdateIN":{"$date":"%s"},"properties.Paddock":"%s", "properties.PaddockID":"%s"}}',
    #                     paste0(substr(date[i],1,10),"T","00:00:00","+1000"), paddock[i], temppad$`_id`)
    #RFIDI <- sprintf('{"$set":{"pdkhist.dateIN.%s":{"$date":"%s"}, "pdkhist.ID.%s":"%s", "pdkhist.name.%s":"%s"}}',
    #                 arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, temppad$`_id`, arrpos, paddock[i])

    #cattle$update(RFIDS, RFIDI)
    #cattle$update(RFIDS, RFIDIlast)

    #add_paddockhistory(RFID = cows$RFID[i], cattle_id = cows$`_id`[i], MTag = cows$Management[i], property = property,
    #                   Paddock = paddock[i], currentPaddock = "TRUE", dateIN = date[i], username = username, password = password)

    }}

}



