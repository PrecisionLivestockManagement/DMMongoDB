#' Update pregnancy information to DataMuster MongoDB database.
#'
#' This function adds individual or groups of cow pregnancy information in the Cattle and CalvingData collections in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_pregdata
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tag number/s
#' @param property the name of the property
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


add_pregdata <- function(RFID = NULL, MTag = NULL, property, date, foetalage = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  calvingdata <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)


  ##### Finding cows #####
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

  if (!(is.null(RFID))){
    checkcows <- paste(unlist(RFID), collapse = '", "' )
    filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
    cows <- cattle$find(query = filtercattle, fields = '{"RFID":true, "stationname":true, "_id":false}')
    cows <- cows%>%filter(stationname == property)

    if (nrow(cows) < length(RFID)) {
      problemcowtags <- as.character(MTag[!(RFID %in% cows$RFID)])
      if (length(problemcowtags) != 0){ #Indicates they are not in the database
        stop(paste0("The following RFID numbers cannot be found in the database. Please check that the MTag numbers are correct and try again: "), problemcowtags)}
    }}



  ##### Add data to CalvingData collection ######
  if(!(is.null(MTag))){
    checkcows <- paste(unlist(MTag), collapse = '", "' )
    filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcows)
    cows <- cattle$find(query = filtercattle, fields = '{"RFID":true, "properties.Management":true, "stationname":true, "_id":true, "properties.foetalagedate":true}')
    cows <- cows%>%filter(stationname == property)

    temp <- calvingdata$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')

    for(i in 1:length(foetalage)){
      if(foetalage[i] != 0){
        temp$RFID <- cows$RFID
        temp$cow_id <- cows$`_id`
        temp$Management <- cows$properties$Management
        temp$stationname <- property
        temp$foetalagedate <- as.POSIXct(date)
        temp$foetalage <- foetalage
        temp$estcalvingdate <- as.POSIXct(date + (41 - foetalage) * 7)
        temp$multiples <- "FALSE"
        temp$prevcalfcheckdate <- as.POSIXct(cows$properties$foetalagedate)
        temp$DoBalgdev <- "FALSE"
        temp$season <- paste(strftime(temp$foetalagedate, format = "%Y"), "/", substr(as.numeric(strftime(temp$foetalagedate, format = "%Y"))+1, 3, 4), sep = "")
        calvingdata$insert(temp)
    }
  }
}



  ##### Add data to Cattle collection ######

    if(!(is.null(MTag))){
      IDS <- sprintf('{"stationname":"%s","properties.Management":"%s"}', property, MTag[i])
      banger <- cattle$find(query= IDS, fields='{"preghist.date":true, "_id":false}')
      arrpos <- length(banger$preghist$date[[1]])

      matchdate <- which(substr(banger$preghist$date[[1]],1,7) == substr(date[i],1,7))

      temp <- data.frame(MTag = MTag, stringsAsFactors = FALSE)
      temp$stationname <- property
      temp$foetalagedate <- as.POSIXct(date)
      temp$foetalage <- foetalage
      temp$estcalvingdate <- as.POSIXct(date + (41 - foetalage) * 7)
      temp$season <- paste(strftime(temp$foetalagedate, format = "%Y"), "/", substr(as.numeric(strftime(temp$foetalagedate, format = "%Y"))+1, 3, 4), sep = "")

      for (i in 1:length(MTag)){
      if(foetalage[i] != 0 & length(matchdate) == 0){
        RFIDI <- sprintf('{"$set":{"preghist.date.%s":{"$date":"%s"}, "preghist.foetalage.%s":"%s", "preghist.estcalvingdate.%s":{"$date":"%s"}}}',
                         arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, foetalage[i], arrpos,
                         paste0(substr(temp$estcalvingdate[i],1,10),"T","00:00:00","+1000"))
        cattle$update(IDS, RFIDI)}
  }
  }


    if(!(is.null(RFID))){
      IDS <- sprintf('{"RFID":"%s"}', RFID[i])
      banger <- cattle$find(query= IDS, fields='{"preghist.date":true, "_id":false}')
      arrpos <- length(banger$preghist$date[[1]])

      matchdate <- which(substr(banger$preghist$date[[1]],1,7) == substr(date[i],1,7))

      temp <- data.frame(RFID = RFID, stringsAsFactors = FALSE)
      temp$stationname <- property
      temp$foetalagedate <- as.POSIXct(date)
      temp$foetalage <- foetalage
      temp$estcalvingdate <- as.POSIXct(date + (41 - foetalage) * 7)
      temp$season <- paste(strftime(temp$foetalagedate, format = "%Y"), "/", substr(as.numeric(strftime(temp$foetalagedate, format = "%Y"))+1, 3, 4), sep = "")

      for (i in 1:length(RFID)){
      if(foetalage[i] != 0 & length(matchdate) == 0){
        RFIDI <- sprintf('{"$set":{"preghist.date.%s":{"$date":"%s"}, "preghist.foetalage.%s":"%s", "preghist.estcalvingdate.%s":{"$date":"%s"}}}',
                         arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, foetalage[i], arrpos,
                         paste0(substr(temp$estcalvingdate[i],1,10),"T","00:00:00","+1000"))
        cattle$update(IDS, RFIDI)}
  }
  }
    }
