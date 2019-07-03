#' Update cow calving information to DataMuster database
#'
#' This function updates individual or groups of cow calving information to the DataMuster database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatecalvinginfo
#' @param RFID this is a list of the cow RFID numbers
#' @param MTag this is a list of the cow management tag numbers
#' @param property the name of the property to add the cattle
#' @param calfRFID the calf's RFID number
#' @param calfMTag the calf's management tag number
#' @param birthDate the cow's date of calving, this has to be in date format
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatecalvinginfo <- function(RFID=NULL, MTag=NULL, property, calfRFID=NULL, calfMTag=NULL, birthDate=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
    infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)
    culls <- mongo(collection = "Culls", db = "DataMuster", url = pass, verbose = T)

    # Check that the property is registered in the database ---------------------------------------------------------------------

    filterstation <- sprintf('{"name":"%s"}', property)
    station <- stations$find(query = filterstation, fields = '{"_id":true}')

    if(nrow(station) == 0) {
      stop('Could not find matching property. Please check spelling and ensure the property is registered in the database.')}


# Check that the RFID numbers are in the correct format and exist in the database

if (!(is.null(RFID))){
if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
  stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

checkcows <- paste(unlist(RFID), collapse = '", "' )

filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
check <- cattle$count(query = filtercattle)

if (check != length(RFID)) {
  stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}
}


 # Check that the MTag numbers exist in the database

    if (!(is.null(MTag))){

      checkcows <- paste(unlist(MTag), collapse = '", "' )

      filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcows)
      check <- cattle$count(query = filtercattle)

      if (check != length(MTag)) {
        stop("One or more of the MTag numbers cannot be found in the database. Please check that the MTag numbers are correct and try again")}
    }

  # Check that progeny RFID numbers are in the correct format and exist in the database---------------------------------------------------------------------

    if (!(is.null(calfRFID))){
    if("TRUE" %in% (nchar(as.character(calfRFID))!= 16)) {
      stop(paste0("One or more of the dam RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

checkcalves <- paste(unlist(calfRFID), collapse = '", "' )

filtercalves <- sprintf('{"RFID":{"$in":["%s"]}}', checkcalves)
check1 <- cattle$find(query = filtercalves, , fields = '{"_id":true}')

if (nrow(check1) == 0) {
  stop("The calf RFID number cannot be found in the database. Please check that the RFID numbers are correct and try again")}
    }

    if (!(is.null(calfMTag) & is.null(calfRFID))){

      checkcalves <- paste(unlist(calfMTag), collapse = '", "' )

      filtercalves <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcalves)
      check1 <- cattle$find(query = filtercalves, fields = '{"_id":true}')

      if (nrow(check1) == 0) {
        stop("The calf MTag cannot be found in the database. Please check that the MTag is correct and try again")}
    }

  #  Update animal information --------------------------

if(!(is.null(RFID))){
    for (p in 1:length(RFID)){

      RFIDS <- sprintf('{"RFID":"%s"}', RFID[p])

      banger <- cattle$find(query= RFIDS, fields='{"calfhist.date":true, "_id":false}')

      arrpos <- length(banger$calfhist$date[[1]])

      matchdate <- which(substr(banger$stwthist$date[[1]],1,7) == substr(birthDate[p],1,17))

      if (length(matchdate) == 0){

      #calfid<- cattle$find(query= sprintf('{"RFID":"%s"}', calfRFID[p]), fields='{"_id":true}') #calfRFID

      # if (nrow(calfid) == 0){print(paste0('The calf RFID ', calfRFID[p], ' is not registered in the database. Calving details have been noted but is not linked in the database'))
      #   RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[p],1,10),"T","00:00:00","+1000"), arrpos, "xxxxxx")
      # }else{

        RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[p],1,10),"T","00:00:00","+1000"), arrpos, check1$`_id`)}
      cattle$update(RFIDS, RFIDI)
      }}

    p<-1
    if(is.null(RFID)){
      for (p in 1:length(MTag)){

        RFIDS <- sprintf('{"properties.Management":"%s"}', MTag[p])

        banger <- cattle$find(query= RFIDS, fields='{"calfhist.date":true, "_id":false}')

        if (nrow(banger) > 1){
          print(paste0("The MTag ", MTag[p], " is registered multiple times in the database. Please check that the MTag is correct and try again. The calving details have not been updated for this animal"))}else{

            arrpos <- length(banger$calfhist$date[[1]])

            matchdate <- which(substr(banger$calfhist$date[[1]],1,7) == substr(birthDate[p],1,7))

            if (length(matchdate) == 0){

              #calfid<- cattle$find(query= sprintf('{"RFID":"%s"}', calfRFID[p]), fields='{"_id":true}') #calfRFID

              # if (nrow(calfid) == 0){print(paste0('The calf RFID ', calfRFID[p], ' is not registered in the database. Calving details have been noted but is not linked in the database'))
              #   RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[p],1,10),"T","00:00:00","+1000"), arrpos, "xxxxxx")
              # }else{

                RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[p],1,10),"T","00:00:00","+1000"), arrpos, check1$`_id`)}
              cattle$update(RFIDS, RFIDI)
            }}}

}





