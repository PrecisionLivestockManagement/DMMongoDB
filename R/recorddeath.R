#' Record a cattle death
#'
#' This function removes individual or groups of cattle from a station. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name recorddeath
#' @param RFID this is a list of cattle RFID numbers
#' @param date provide the date that the animal died, this has to be in date format. Default is today's date.
#' @param cause provide the cause of death
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


recorddeath <- function(RFID, date=NULL, cause=NULL, property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}
  if(is.null(cause)){cause <- "xxxxxx"}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(cause) == 1){cause <- rep(cause, length = length(RFID))}

  # Check that the RFID numbers are in the correct format and exist in the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  #checkcows <- paste(unlist(RFID), collapse = '", "' )

  cows <- cattlesearch(RFID)

  if (nrow(cows) != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}


  for (i in 1:length(cows$RFID)){

    RFIDS <- sprintf('{"RFID":"%s"}', cows$RFID[i])

    RFIDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "geometry.coordinates.0":"%s", "geometry.coordinates.1":"%s",
                     "properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.deathcause":"%s", "properties.deathDate":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                     "xxxxxx", "xxxxxx", "FALSE", cows$stationname[i], 0.0, 0.0, "xxxxxx", "xxxxxx", cause[i], paste0(substr(date[i],1,10),"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")

      cattle$update(RFIDS, RFIDI)}

  movecattle(property = property, username = username, password = password)

}



