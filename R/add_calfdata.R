#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function adds individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_calfdata
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tag number/s
#' @param calfRFID the calf's RFID tag number
#' @param calfMTag the calf's management tag number
#' @param property the name of the property
#' @param date the calving date in date format
#' @param weight the calf's birthweight
#' @param multiples if the calf is a twin or single, TRUE or FALSE. Default is FALSE
#' @param sex the calf's sex
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


add_calfdata <- function(RFID = NULL, MTag = NULL, calfRFID = NULL, calfMTag, property, date, weight = NULL, multiples = NULL, sex = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  calvingdata <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)


  ##### Finding cows #####
  if (!(is.null(RFID))){
    checkcows <- paste(unlist(RFID), collapse = '", "' )
    filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
    } else {
      checkcows <- paste(unlist(MTag), collapse = '", "' )
      filtercattle <- sprintf('{"stationname":"%s", "properties.Management":{"$in":["%s"]}}', property, checkcows)}

  cows <- cattle$find(query = filtercattle, fields = '{"RFID":true, "properties.Management":true, "stationname":true, "_id":true,
                        "properties.foetalagedate":true, "properties.Paddock":true}')

  if (nrow(cows) < length(calfMTag)) {
    if (!(is.null(RFID))){
      problemcowtags <- as.character(RFID[!(RFID %in% cows$RFID)])
      } else {
        problemcowtags <- as.character(MTag[!(MTag %in% cows$properties$Management)])}
    if (length(problemcowtags) != 0){
      stop(paste0("The following Tag numbers cannot be found in the database. Please check that the Tag numbers are correct and try again: "), problemcowtags)}
  }



  ###### Add calves to Cattle collection #####
for (i in 1:length(calfMTag)){
  if (!(is.null(RFID))){
    cow <- cows[cows$RFID == RFID[i],]
    } else {
      cow <- cows[cows$properties$Management == MTag[i],]}

  if (!is.null(calfRFID) & !is.null(weight) & !is.null(sex)){
    add_cattle(RFID = calfRFID[i], MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock,
               sex = sex[i], birthWeight = weight[i],
               weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else if(!is.null(calfRFID) & !is.null(weight) & is.null(sex)){
      add_cattle(RFID = calfRFID[i], MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock, birthWeight = weight[i],
                 weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else if (!is.null(calfRFID) & is.null(weight) & !is.null(sex)){
      add_cattle(RFID = calfRFID[i], MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock, sex = sex[i],
                 weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else if (is.null(calfRFID) & !is.null(weight) & !is.null(sex)) {
      add_cattle(RFID = "xxx xxxxxxxxxxxx", MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock, birthWeight = weight[i],
                 sex = sex[i],
                 weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else if (is.null(calfRFID) & !is.null(weight) & is.null(sex)){
      add_cattle(RFID = "xxx xxxxxxxxxxxx", MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock, birthWeight = weight[i],
                 weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else if (is.null(calfRFID) & is.null(weight) & !is.null(sex)){
      add_cattle(RFID = "xxx xxxxxxxxxxxx", MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock, sex = sex[i],
                 weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
    } else {
    add_cattle(RFID = "xxx xxxxxxxxxxxx", MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock,
               weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID, damMTag = cow$properties$Management, username = username, password = password)
  }
}


  ##### Add cow calving data to Cattle collection #####
checkcalves <- paste(unlist(calfMTag), collapse = '", "' )
filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcalves)
calves <- cattle$find(query = filtercattle, fields = '{"RFID":true, "properties.Management":true, "stationname":true, "_id":true}')
calves <- calves%>%filter(stationname == property)

if (!(is.null(RFID))){
  for (i in 1:length(RFID)){
    calfid <- calves$`_id`[calves$properties$Management == calfMTag[i]]
    IDS <- sprintf('{"stationname":"%s","RFID":"%s"}', property, RFID[i])
    banger <- cattle$find(query= IDS, fields='{"calfhist.date":true, "_id":false}')
    arrpos <- length(banger$calfhist$date[[1]])
    matchdate <- which(substr(banger$calfhist$date[[1]],1,7) == substr(date[i],1,7))

    if (length(matchdate) == 0){
      RFIDIlast <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
      cattle$update(IDS, RFIDIlast)
    }
  }
} else {
  calfid <- calves$`_id`[calves$properties$Management == calfMTag[i]]
  IDS <- sprintf('{"stationname":"%s","properties.Management":"%s"}', property, MTag[i])
  banger <- cattle$find(query= IDS, fields='{"calfhist.date":true, "_id":false}')
  arrpos <- length(banger$calfhist$date[[1]])
  matchdate <- which(substr(banger$calfhist$date[[1]],1,7) == substr(date[i],1,7))

  if (length(matchdate) == 0){
    RFIDIlast <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
    cattle$update(IDS, RFIDIlast)
  }
}


  ##### Add data to CalvingData collection ######
  RFID <- cows$RFID
  month <- as.numeric(strftime(date, format = "%m"))
  season <- ifelse(month >= 7, strftime(date, format = "%Y"), as.numeric(strftime(date, format = "%Y"))-1)
  season <- paste0(season, "/", substr(as.numeric(season)+1, 3, 4))

for (i in 1:length(RFID)){
  IDS <- sprintf('{"RFID":"%s"}', RFID[i])
  match <- calvingdata$find(query = IDS, fields = '{"RFID":true, "season":true, "calf_id":true}')


  if(nrow(match) == 0){
    for (i in 1:length(RFID)){
      template <- calvingdata$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')
      template$RFID <- RFID[i]
      template$cow_id <- cows$`_id`[i]
      template$Management <- cows$properties$Management[i]
      template$stationname <- property
      template$calvingdate <- as.POSIXct(paste(date[i], "00:00:00"))
      template$season <- season

      calvingdata$insert(template)

      if(is.null(calfMTag)){} else {
        arrpos <- length(match$calf_id[[i]])
        IDI <- sprintf('{"$set":{"calf_id.%s":"{"%s"}"}}', arrpos, calves$`_id`[i])
        calvingdata$update(IDS, IDI)
        }
    }
  } else {
    for (i in 1:length(RFID)){
      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
      IDS <- sprintf('{"RFID":"%s", "season":"%s"}', RFID[i], season[i])

      if(is.null(date)){} else {
        IDI <- sprintf('{"$set":{"calvingdate":{"$date":"%s"}}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
        calvingdata$update(IDS, IDI)}
      if(is.null(multiples)){} else {
        IDI <- sprintf('{"$set":{"multiples":"%s"}}', multiples[i])
        calvingdata$update(IDS, IDI)}
      if(is.null(calfMTag)){} else {
        arrpos <- length(calves$`_id`[[1]])
        IDI <- sprintf('{"$set":{"calf_id.%s":"{"%s"}"}}', arrpos, calves$`_id`[i])
        calvingdata$update(IDS, IDI)}
    }
  }
      }
}
