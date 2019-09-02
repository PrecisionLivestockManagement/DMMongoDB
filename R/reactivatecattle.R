#' Reactivate cattle on a station
#'
#' This function reactivates individual or groups of cattle that have previously been removed frp, a station. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name reactivatecattle
#' @param RFID this is a list of cattle RFID numbers
#' @param property this is the name of the property
#' @param date provide the date that the animal left the station, this has to be in date format. Default is today's date.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


reactivatecattle <- function(RFID, property, paddock, date=NULL, replacevalues=NULL, username=NULL, password=NULL){

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

  cows <- cattlesearch(RFID, username = username, password = password)

  if (nrow(cows) != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  stationinfo <- stationinfo(property, username = username, password = password)


  for (i in 1:length(cows$RFID)){

    if (cows$active[i] == FALSE){

    RFIDS <- sprintf('{"RFID":"%s"}', cows$RFID[i])

    banger <- cattle$find(query= RFIDS, fields='{"pdkhist.dateOUT":true, "_id":false}')
    arrpos <- length(banger$pdkhist$dateOUT[[1]])

    RFIDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "properties.exitDate":{"$date":"%s"}}}',
    property, stationinfo$`_id`, "TRUE", "xxxxxx", paste0("1970-01-01","T","00:00:00.000","Z"))

    cattle$update(RFIDS, RFIDI)

    if (replacevalues == "TRUE"){

    cattle$update(RFIDS, '{"$set":{"pdkhist.dateOUT":[]}}')

    tempdates <- banger$pdkhist$dateOUT[[1]][1:length(banger$pdkhist$dateOUT[[1]])-1]

    if (length(tempdates) != 0){

    for (k in 1:length(tempdates)){

    RFIDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', k-1, paste0(tempdates[k],"T","00:00:00","+1000"))

    cattle$update(RFIDS, RFIDL)
    }}
    }

    updatepaddock(cows$RFID[i], property = property, paddock = paddock[i], date = date[i], username = username, password = password)

    if (replacevalues == "FALSE"){

      banger <- cattle$find(query= RFIDS, fields='{"pdkhist.dateOUT":true, "_id":false}')
      arrpos <- length(banger$pdkhist$dateOUT[[1]])

      cattle$update(RFIDS, '{"$set":{"pdkhist.dateOUT":[]}}')

      tempdates <- banger$pdkhist$dateOUT[[1]][1:length(banger$pdkhist$dateOUT[[1]])-1]

      if (length(tempdates) != 0){

        for (k in 1:length(tempdates)){

          RFIDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', k-1, paste0(tempdates[k],"T","00:00:00","+1000"))

          cattle$update(RFIDS, RFIDL)
        }}
    }

    }}

}



