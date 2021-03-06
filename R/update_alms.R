#' Update cattle allocation to ALMS units in the DataMuster MongoDB database.
#'
#' This function updates allocates individual or groups of cattle to ALMS units in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_alms
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param date the date that the new paddock allocation was applied in date formate, default is today's date
#' @param almsid the asset_id of the ALMS unit the cattle are to be allocated
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the cattle have been successfully allocated to an ALMS unit
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


update_alms <- function(RFID, property, alms, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  #paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.Date(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}
  if (length(alms) == 1){alms <- rep(alms, length = length(RFID))}


  # Check that the RFID numbers are in the correct format and exist in the database

  if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
    stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

  checkcows <- paste(unlist(RFID), collapse = '", "' )

  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
  check <- cattle$count(query = filtercattle)

  if (check != length(RFID)) {
      stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}

  # Check that the alms exists in the database

  checkalms <- paste(unlist(alms), collapse = '", "' )

  filteralms <- sprintf('{"stationname":"%s", "properties.asset_id":{"$in":["%s"]}}', property, checkalms)

  unit <- infs$find(query = filteralms, fields = '{"_id":true, "properties.asset_id":true}')

    if(nrow(unit) != 1) {
      stop('Could not find matching alms unit. Please check the spelling and ensure the alms is registered in the database')
    }

  for (i in 1:length(RFID)){

    #Update Cattle collection
          RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
          banger <- cattle$find(query= RFIDS, fields='{"almshist.dateON":true, "_id":false}')
          rip <- length(banger$almshist$dateON[[1]])
          IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             rip, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), rip, unit$`_id`, rip, unit$properties$asset_id)
          IDIlast <- sprintf('{"$set":{"properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                             "TRUE", unit$`_id`, unit$properties$asset_id)
          cattle$update(RFIDS, IDI)
          cattle$update(RFIDS, IDIlast)

          #Update ALMSHistory collection
          cows <- get_cattle(RFID = RFID[i], property = property, fields = c("RFID", "properties.Management", "stationname"))

          add_almshistory(RFID = cows$RFID, cattle_id = cows$`_id`, MTag = cows$Management, property = cows$stationname, ALMS = alms[i],
                            currentALMS = "TRUE", dateON = substr(date[i],1,10), dateOFF = NULL, username = username, password = password)
          }
  }



