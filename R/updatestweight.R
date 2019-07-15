#' Update static weight
#'
#' This function inserts a new static weight for individual or groups of cattle based on a list of RFID values. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatestweight
#' @param RFID this is a list of the cattle RFID numbers
#' @param weight this is a list of the cattle weights
#' @param date provide the date that the weight was measured, this has to be in date format. Default is today's date.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the weights been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatestweight <- function(RFID, weight, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.Date(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}


  # Checks that the RFID numbers are in the correct format and are registered in the database

  checkRFID <- paste(unlist(RFID), collapse = '", "' )

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the previous RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkRFID)
  check <- cattle$count(query = filtercattle)

  if (check != length(RFID)) {

    stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}


  for (i in 1:length(RFID)){

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

    dat <- date[i]
    wt <- weight[i]

    wt <- ifelse(wt == "" | is.na(wt),0,wt)

    if(!(is.na(dat))){

        banger <- cattle$find(query = RFIDS, fields='{"stwthist.date":true, "stwthist.weight":true, "_id":false}')

        if (length(banger$stwthist$date[[1]]) != 0){dates <- as.Date(banger$stwthist$date[[1]], tz = "Australia/Brisbane")
        matchdate <- which(dates == dat)
        matchwt <- banger$stwthist$weight[[1]][matchdate]}

        if (exists("matchdate") && wt %in% matchwt){}else{

          arrpos <- length(banger$stwthist$date[[1]])
          RFIDI <- sprintf('{"$set":{"properties.stweight":%s, "properties.stwtdate":{"$date":"%s"}}}', wt, paste0(substr(dat,1,10),"T","00:00:00","+1000"))
          RFIDIlast <- sprintf('{"$set":{"stwthist.date.%s":{"$date":"%s"}, "stwthist.weight.%s":%s}}', arrpos, paste0(substr(dat,1,10),"T","00:00:00","+1000"), arrpos, wt)

      cattle$update(RFIDS, RFIDI)
      cattle$update(RFIDS, RFIDIlast)}}}

}


