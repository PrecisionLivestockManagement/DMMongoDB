#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function adds individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_calfdata
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tag number/s
#' @param calfRFID the calf's RFID tag number
#' @param calfMTag the calf's management tag number
#' @param calfweight the calf's birthweight
#' @param calfsex the calf's sex
#' @param property the name of the property
#' @param date the calving date in date format
#' @param multiples if the calf is a twin or single, TRUE or FALSE. Default is FALSE
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

  calfRFID <- ifelse(is.null(calfRFID), rep("xxx xxxxxxxxxxxx", length(calfMTag)), calfRFID)
  if (length(date) == 1){date <- rep(date, length = length(calfMTag))}
  weight <- ifelse(is.null(weight), rep(0, length(calfMTag)), weight)
  multiples <- ifelse(is.null(multiples), rep("FALSE", length(calfMTag)), multiples)
  sex <- ifelse(is.null(sex), rep("xxxxxx", length(calfMTag)), sex)

  ##### Finding cows #####
  cows <- get_cattle(RFID = RFID, MTag = MTag, property = property,
                     fields = c("RFID", "properties.Management", "stationname", "properties.foetalagedate", "properties.Paddock"),
                     username = NULL, password = NULL)

  if (nrow(cows) < length(calfMTag)) {
    if (!(is.null(RFID))){
      problemcowtags <- as.character(RFID[!(RFID %in% cows$RFID)])
      } else {
        problemcowtags <- as.character(MTag[!(MTag %in% cows$properties$Management)])}
    if (length(problemcowtags) != 0){
      stop(paste0("The following Tag numbers cannot be found in the database. Please check that the Tag numbers are correct and try again: "), problemcowtags)}
  }

  for (i in 1:length(calfMTag)){
    if (!(is.null(RFID))){
      cow <- cows[cows$RFID == RFID[i],]
    } else {
      cow <- cows[cows$properties$Management == MTag[i],]}

    ##### Add calf to Cattle collection #####
    add_cattle(RFID = calfRFID[i], MTag = calfMTag[i], category = "growing", property = property, paddock = cow$properties$Paddock,
               sex = sex[i], birthWeight = weight[i], weaned = "FALSE", date = date[i], DOB = date[i], damRFID = cow$RFID,
               damMTag = cow$properties$Management, username = username, password = password)


    ##### Add cow calving data to Cattle collection #####
    IDS <- sprintf('{"RFID":"%s"}', cow$RFID)
    RFIDIlast <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
    cattle$update(IDS, RFIDIlast)


    ##### Add data to CalvingData collection #####
    calf <- get_cattle(property = property, MTag = calfMTag[i], username = username, password = password)

    month <- as.numeric(strftime(date[i], format = "%m"))
    season <- ifelse(month >= 5, strftime(date[i], format = "%Y"), as.numeric(strftime(date[i], format = "%Y"))-1)
    season <- paste0(season, "/", substr(as.numeric(season)+1, 3, 4))

    IDSI <- sprintf('{"RFID":"%s", "season":"%s"}', cow$RFID, season)
    match <- calvingdata$find(query = IDSI, fields = '{"RFID":true, "season":true, "calf_id":true}')

    if(nrow(match) == 0){

      template <- calvingdata$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')

      template$RFID <- cow$RFID
      template$cow_id <- cow$`_id`
      template$Management <- cow$Management
      template$stationname <- property
      template$season <- season

      calvingdata$insert(template)}

    match <- calvingdata$find(query = IDSI, fields = '{"RFID":true, "season":true, "calf_id":true}')

    arrpos <- length(match$calf_id[[1]])

    RFIDS <- sprintf('{"RFID":"%s", "season":"%s"}', cow$RFID, season)
    RFIDI <- sprintf('{"$set":{"calvingdate":{"$date":"%s"}, "multiples":"%s", "calf_id.%s":"%s"}}',
                     paste0(substr(date[i],1,10),"T","00:00:00","+1000"), multiples[i], arrpos, calf$`_id`)
    calvingdata$update(RFIDS, RFIDI)
  }
}
