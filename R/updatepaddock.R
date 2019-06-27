#' Update cattle allocations to paddocks
#'
#' This function allocates individual or groups of cattle to paddocks. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatepaddock
#' @param RFID this is a list of cattle RFID numbers
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param date provide the date that the new paddock allocation was applied, this has to be in date format. Default is today's date.
#' @param addtoherd this indicates whether or not there are cattle already allocated to the paddock. use addtoherd = TRUE to ignore.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatepaddock <- function(RFID, property, paddock, date=NULL, username=NULL, password=NULL){

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

  if (check != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  # Check that the paddocks exist in the database

  checkpads <- paste(unlist(paddock), collapse = '", "' )

  filterpaddock <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, checkpads)

  pad <- paddocks$find(query = filterpaddock, fields = '{"_id":true, "geometry":true, "paddname":true, "properties.hectares":true}')

    if(nrow(pad) != length(unique(paddock))) {
      stop('Could not find matching paddock. Please check the spelling and ensure the paddock is registered in the database')
    }

  # Check for WoW infrastructure in new paddocks

  filterinfs <- sprintf('{"stationname":"%s", "paddock":{"$in":["%s"]}, "properties.type":"%s"}', property, checkpads, "Walk-over-Weighing Unit")

  inf <- infs$find(query = filterinfs, fields = '{"_id":true, "paddock":true, "properties.asset_id":true}')

  for (i in 1:length(RFID)){

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

          banger <- cattle$find(query= RFIDS, fields='{"properties.Paddock":true,"properties.ALMS":true,"properties.ALMSID":true,"pdkhist.dateIN":true, "pdkhist.dateOUT":true, "almshist.dateON":true, "almsist.dateOFF":true, "_id":false}')
          arrpos <- length(banger$pdkhist$dateIN[[1]])
          arrpos1 <- length(banger$pdkhist$dateOUT[[1]])

          if (banger$properties$Paddock != paddock[i]){
            temppad <- pad[which(pad$paddname == paddock[i]),]

          RFIDIlast <- sprintf('{"$set":{"properties.Paddock":"%s", "properties.PaddockID":"%s"}}', paddock[i], temppad$`_id`)
          RFIDI <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}, "pdkhist.dateIN.%s":{"$date":"%s"}, "pdkhist.name.%s":"%s", "pdkhist.ID.%s":"%s"}}', arrpos1, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), arrpos, paddock[i], arrpos, temppad$`_id`)

      cattle$update(RFIDS, RFIDI)
      cattle$update(RFIDS, RFIDIlast)

          ALMS <- paddock[i] %in% inf$paddock[[1]]

          arrpos2 <- length(banger$almshist$dateON[[1]])
          arrpos3 <- length(banger$almshist$dateOFF[[1]])

          if (ALMS == "FALSE"){

             if (banger$properties$ALMS == "TRUE"){

              IDI <- sprintf('{"$set":{"almshist.dateOFF.%s":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             arrpos3, paste0(substr(date[i],1,10),"T","00:00:00","+1000"),"FALSE", "xxxxxx", "xxxxxx")}}

          if (ALMS == "TRUE"){

            WOW <- inf[which(inf$paddock[[1]] == paddock[i]),]

              IDI <- sprintf('{"$set":{"properties.ALMSdateON":{"$date":"%s"}, "properties.ALMSdateOFF":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             paste0(substr(date[i],1,10),"T","00:00:00","+1000"), paste0("1970-01-01","T","10:00:00","+1000"), "TRUE", WOW$`_id`, WOW$properties$asset_id)}

          cattle$update(RFIDS, IDI)
}}

movecattle(property = property, paddock = unique(paddock), username = username, password = password)

}



