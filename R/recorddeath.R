#' Record a cattle death on the DataMuster MongoDB database.
#'
#' This function removes individual or groups of cattle from a station on the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name recorddeath
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tags
#' @param property the name of the property to search for
#' @param date the date that the animal died in date format, default is today's date
#' @param cause the cause of death
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the cow has successfully been assigned a dead status
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


recorddeath <- function(RFID, MTag, property, date=NULL, cause=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddockhistory <- mongo(collection = "PaddockHistory", db = "DataMuster", url = pass, verbose = T)
  almshistory <- mongo(collection = "ALMSHistory", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}
  if(is.null(cause)){cause <- "xxxxxx"}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(cause) == 1){cause <- rep(cause, length = length(RFID))}

  # Check that the RFID numbers are in the correct format and exist in the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  #checkcows <- paste(unlist(RFID), collapse = '", "' )

  cows <- get_cattle(RFID = RFID, MTag = MTag, property = property, username = username, password = password,
                     fields = c("RFID", "properties.Management", "active",
                                "stationname", "properties.Paddock", "properties.ALMS",
                                "properties.category", "properties.breed", "properties.sex"))

  # cows <- cattlesearch(RFID, username = username, password = password)

  if (nrow(cows) != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  for (i in 1:length(cows$RFID)){

    # Update Cattle properties
    if (RFID[i] != "xxx xxxxxxxxxxxx"){

      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{

        RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

    #Update Cattle PdkHist
    banger <- cattle$find(query= RFIDS, fields='{"pdkhist.dateOUT":true, "_id":true}')
    arrpos <- length(banger$pdkhist$dateOUT[[1]])
    RFIDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
    cattle$update(RFIDS, RFIDL)

    #Update Cattle ALMSHist
    if(cows$ALMS[i] == "TRUE"){
      banger1 <- cattle$find(query= RFIDS, fields='{"almshist.dateOFF":true, "_id":true}')
      arrpos1 <- length(banger1$almshist$dateOFF[[1]])
      RFIDLI <- sprintf('{"$set":{"almshist.dateOFF.%s":{"$date":"%s"}}}', arrpos1, paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
      cattle$update(RFIDS, RFIDLI)}

    # Update PaddockHistory collection
    padhist <- get_paddockhistory(cattle_id = banger$`_id`, currentPaddock = "TRUE", username = username, password = password)
    IDI <- sprintf('{"_id":{"$oid":"%s"}}', padhist$`_id`)
    IDS <- IDSI <- sprintf('{"$set":{"currentPaddock":"%s", "dateOUT":{"$date":"%s"}}}', "FALSE", paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
    paddockhistory$update(IDI, IDS)

    # Update ALMSHistory collection
    if(cows$ALMS[i] == "TRUE"){
      almshist <- get_almshistory(cattle_id = banger$`_id`, currentALMS = "TRUE", username = username, password = password)
      IDII <- sprintf('{"_id":{"$oid":"%s"}}', almshist$`_id`)
      IDSI <- sprintf('{"$set":{"currentALMS":"%s", "dateOFF":{"$date":"%s"}}}', "FALSE", paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
      almshistory$update(IDII, IDSI)}

    RFIDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "geometry.coordinates.0":%s, "geometry.coordinates.1":%s,
                     "properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.deathcause":"%s", "properties.deathDate":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                     "xxxxxx", "xxxxxx", "FALSE", cows$stationname[i], 0.0, 0.0, "xxxxxx", "xxxxxx", cause[i], paste0(substr(date[i],1,10),"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")
    cattle$update(RFIDS, RFIDI)
  }

  #update_cattlecoords(property = unique(cows$stationname), paddock = unique(cows$Paddock), username = username, password = password)

}



