#' Update cattle allocations to alms units
#'
#' This function allocates individual or groups of cattle to alms units. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatepaddock
#' @param RFID this is a list of cattle RFID numbers
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param date provide the date that the new paddock allocation was applied, this has to be in date format. Default is today's date.
#' @param almsid this is the asset_id of the alms unti the cattle are to be allocated.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatealms <- function(RFID, property, alms, date=NULL, username=NULL, password=NULL){

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

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

          banger <- cattle$find(query= RFIDS, fields='{"almshist.dateON":true, "_id":false}')

          rip <- length(banger$almshist$dateON[[1]])


         IDI <- sprintf('{"$set":{"almshist.dateON.%s":{"$date":"%s"}, "almshist.ID.%s":"%s", "almshist.asset_id.%s":"%s"}}',
                             rip, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), rip, unit$`_id`, rip, unit$properties$asset_id)

         IDIlast <- sprintf('{"$set":{"properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                        "TRUE", unit$`_id`, unit$properties$asset_id)


          cattle$update(RFIDS, IDI)
          cattle$update(RFIDS, IDIlast)}
  }



