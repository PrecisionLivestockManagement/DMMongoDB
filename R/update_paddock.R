#' Update cattle allocations to paddocks in the DataMuster MongoDB database.
#'
#' This function allocates individual or groups of cattle to paddocks in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_paddock
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param date the date the new paddock allocation was applied in date format, default is today's date
#' @param addtoherd this indicates whether or not there are cattle already allocated to the paddock. Use addtoherd = TRUE to ignore
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the paddocks have been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


update_paddock <- function(RFID, property, paddock, MTag = NULL, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(paddock) == 1){paddock <- rep(paddock, length = length(RFID))}


  # Check that the RFID numbers are in the correct format and exist in the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  checkcows <- paste(unlist(RFID), collapse = '", "' )

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
  check <- cattle$count(query = filtercattle)

  #if (check != length(RFID)) {
  #    stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  # Check that the paddocks exist in the database

  checkpads <- paste(unlist(paddock), collapse = '", "' )

  filterpaddock <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, checkpads)

  pad <- paddocks$find(query = filterpaddock, fields = '{"_id":true, "geometry":true, "paddname":true, "properties.hectares":true}')

    if(nrow(pad) != length(unique(paddock))) {
      stop('Could not find matching paddock. Please check the spelling and ensure the paddock is registered in the database')
    }

  # Check for WoW infrastructure in new paddocks

  filterinfs <- sprintf('{"stationname":"%s", "paddock":{"$in":["%s"]}, "properties.type":"%s"}', property, checkpads, "Walk-over-Weighing Unit")

  inf <- infs$find(query = filterinfs, fields = '{"_id":true, "paddock":true, "properties.asset_id":true, "properties.datarecording":true}')

  for (i in 1:length(RFID)){

    if (RFID[i] != "xxx xxxxxxxxxxxx"){

      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{

        RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

          banger <- cattle$find(query= RFIDS, fields='{"properties.Paddock":true,"properties.ALMS":true,"properties.ALMSID":true,"pdkhist.name":true, "pdkhist.dateIN":true, "pdkhist.dateOUT":true, "almshist.dateON":true, "almshist.dateOFF":true, "_id":false}')
          arrpos <- length(banger$pdkhist$dateIN[[1]])
          arrpos1 <- length(banger$pdkhist$dateOUT[[1]])

          prevpaddock <- banger$properties$Paddock

      # If current paddock is different to new padock, continue

          if (banger$properties$Paddock != paddock[i]){

        # Update paddock

          temppad <- pad[which(pad$paddname == paddock[i]),]

          RFIDIlast <- sprintf('{"$set":{"properties.PaddockdateIN":{"$date":"%s"},"properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.PrevPaddock":"%s"}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"), paddock[i], temppad$`_id`, prevpaddock)
          RFIDI <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}, "pdkhist.dateIN.%s":{"$date":"%s"}, "pdkhist.ID.%s":"%s"}}', arrpos1, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paddock[i], arrpos, temppad$`_id`)

      cattle$update(RFIDS, RFIDI)
      cattle$update(RFIDS, RFIDIlast)

        # Does the new paddock have an ALMS? TRUE or FALSE

          ALMS <- paddock[i] %in% inf$paddock[[1]]

          arrpos2 <- length(banger$almshist$dateON[[1]])
          arrpos3 <- length(banger$almshist$dateOFF[[1]])

          # If the new paddock does not have an ALMS or if the ALMS is not active and the animal is currently allocated to an ALMS unit, remove the animal from that unit..

          if (ALMS == "FALSE" ||
              ALMS == "TRUE" & inf$properties$datarecording == "FALSE"){

             if (banger$properties$ALMS == "TRUE"){

              IDI <- sprintf('{"$set":{"almshist.dateOFF.%s":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             arrpos3, paste0(substr(date[i],1,10),"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")

              cattle$update(RFIDS, IDI)
              }}

          # If the new paddock does have an active ALMS unit..

          if (ALMS == "TRUE" && inf$properties$datarecording == "TRUE"){

            WOW <- inf[which(inf$paddock[[1]] == paddock[i]),]

            IDIlast <- sprintf('{"$set":{"properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             "TRUE", WOW$`_id`, WOW$properties$asset_id)

            # If the animal is currently allocated to an ALMS unit...

            if (banger$properties$ALMS == "TRUE"){

              IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.dateOFF.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             arrpos2, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos3, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos2, WOW$`_id`, arrpos2, WOW$properties$asset_id)}

            # If the animal is not currently allocated to an ALMS unit...

            if (banger$properties$ALMS == "FALSE"){

              IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             arrpos2, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos2, WOW$`_id`, arrpos2, WOW$properties$asset_id)}

          cattle$update(RFIDS, IDIlast)
          cattle$update(RFIDS, IDI)
          }


          }}

movecattle(property = property, paddock = unique(paddock), username = username, password = password)

}


