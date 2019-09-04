#' Update pregnancy diagnosis results
#'
#' This function inserts a new static weight for individual or groups of cattle based on a list of RFID values. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatepregtest
#' @param RFID this is a list of the cattle RFID numbers
#' @param foetalage this is a list of the foetal age in weeks
#' @param date provide the date that the pregnancy diagnosis was measured, this has to be in date format. Default is today's date.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the pregnancy results been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatepregtest <- function(RFID, foetalage, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  meangestation <- 41

  if(is.null(date)){date <- Sys.Date()}else{date <- as.Date(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}

  # Checks that the RFID numbers are in the correct format and are registered in the database

  #checkRFID <- paste(unlist(RFID), collapse = '", "' )

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the previous RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  #filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkRFID)
  #check <- cattle$count(query = filtercattle)

  check <- RFIDsearch(RFID, username = username, password = password)

  if (nrow(check) != length(RFID)) {

    stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}


  for (i in 1:length(RFID)){

    match <- RFIDsearch(RFID[i], username = username, password = password)

    RFIDS <- sprintf('{"RFID":"%s"}', match$RFID)

    dat <- date[i]
    age <- foetalage[i]

    age <- ifelse(age == "" | is.na(age), 0, age)

    if (age == 0){
      calvedate <- "1970-01-01T10:00:00+1000"} else{
      calving <- dat + meangestation*7 - age*7
      calvedate <- paste0(calving,"T00:00:00+1000")}

    if(!(is.na(dat))){

        banger <- cattle$find(query = RFIDS, fields='{"preghist.date":true, "preghist.foetalage":true, "_id":false}')

        if (length(banger$preghist$date[[1]]) != 0){dates <- as.Date(banger$preghist$date[[1]], tz = "Australia/Brisbane")
        matchdate <- which(dates == dat)
        matchage <- banger$preghist$foetalage[[1]][matchdate]}

        if (exists("matchdate") && age %in% matchage){}else{

          arrpos <- length(banger$preghist$date[[1]])
          RFIDI <- sprintf('{"$set":{"properties.estcalvingdate":{"$date":"%s"}}}', calvedate)
          RFIDIlast <- sprintf('{"$set":{"preghist.date.%s":{"$date":"%s"}, "preghist.foetalage.%s":%s, "preghist.estcalvingdate.%s":{"$date":"%s"}}}', arrpos, paste0(substr(dat,1,10),"T","00:00:00","+1000"), arrpos, age, arrpos, calvedate,1,10)

      cattle$update(RFIDS, RFIDI)
      cattle$update(RFIDS, RFIDIlast)
        }
    }

    if(exists("matchdate")){rm(matchdate)}
       if(exists("matchage")){rm(matchage)}

    }

}


