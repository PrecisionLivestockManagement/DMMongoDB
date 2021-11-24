#' Update cattle allocations to paddocks in the DataMuster MongoDB database.
#'
#' This function allocates individual or groups of cattle to paddocks in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_paddock
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param paddock the name of the paddock to allocate cattle to
#' @param MTag a list of cattle management tags
#' @param date the date the new paddock allocation was applied in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the paddocks have been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


update_paddock <- function(RFID, property, paddock, MTag, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)


  # ---------------------------------------------------------------------------------------------------------------------------------------
  # Load in collections
  # ---------------------------------------------------------------------------------------------------------------------------------------
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)
  paddockhistory <- mongo(collection = "PaddockHistory", db = "DataMuster", url = pass, verbose = T)
  almshistory <- mongo(collection = "ALMSHistory", db = "DataMuster", url = pass, verbose = T)


  # ---------------------------------------------------------------------------------------------------------------------------------------
  # Create defaults and reps for date and paddock
  # ---------------------------------------------------------------------------------------------------------------------------------------
  if(is.null(date)){
    date <- Sys.Date()
    dater <- date
  }else{
    dater <- date
    date <- as.POSIXct(date)
  }

  if(length(date) == 1){
    date <- rep(date, length = length(RFID))
  }

  if(length(paddock) == 1){
    paddock <- rep(paddock, length = length(RFID))
  }


  # ---------------------------------------------------------------------------------------------------------------------------------------
  # RFID and paddock checks
  # ---------------------------------------------------------------------------------------------------------------------------------------
  problemRFIDs <- data.frame(RFID) %>%
    filter(nchar(RFID) != 16)
  if(nrow(problemRFIDs) > 0){
    stop(paste("The following RFID numbers are not in the correct format:", problemRFIDs$RFID))
  }

  search <- paste(unlist(RFID), collapse = '", "')
  search <- sprintf('{"RFID":{"$in":["%s"]}}', search)
  search <- cattle$find(query = search, fields = '{"_id":false, "RFID":true}')
  notindbRFIDs <- data.frame(RFID) %>%
    filter(!(RFID %in% search$RFID))
  if(nrow(notindbRFIDs) > 0){
    stop(paste("The following RFID numbers are not in the database:", notindbRFIDs$RFID))
  }

  search <- paste(unlist(paddock), collapse = '", "')
  search <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, search)
  search <- paddocks$find(query = search, fields = '{"_id":false, "paddname":true}')
  notindbpaddocks <- data.frame(paddock) %>%
    filter(!(paddock %in% search$paddname))
  if(nrow(notindbpaddocks) > 0){
    stop(paste("The following paddocks are not in the database:", notindbpaddocks$paddock))
  }


  # ---------------------------------------------------------------------------------------------------------------------------------------
  # Update data
  # ---------------------------------------------------------------------------------------------------------------------------------------
  for(i in 1:length(RFID)){
    # -----------------------------------
    # Cattle
    # -----------------------------------
    if(RFID[i] != "xxx xxxxxxxxxxxx"){
      RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
    } else {
      RFIDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])
    }
    cattledf <- cattle$find(query = RFIDS, fields = '{"_id":true, "RFID":true, "properties.Management":true, "properties.Paddock":true, "properties.PaddockID":true, "properties.PaddockdateIN":true, "properties.PrevPaddock":true, "properties.ALMS":true, "properties.ALMSasset_id":true}')

    if(cattledf$properties$Paddock != paddock[i]){
      if(as.Date(strftime(cattledf$properties$PaddockdateIN, format = "%Y-%m-%d")) > as.Date(date)[i]){
        stop(paste("The date allocated to", RFID[i], "is before the previous paddock date in the database."))
      } else {
        search <- sprintf('{"stationname":"%s", "paddname":"%s"}', property, paddock[i])
        pdkdf <- paddocks$find(query = search, fields = '{"_id":true, "paddname":true}')

        update <- sprintf('{"$set":{"properties.Paddock":"%s", "properties.PaddockID":"%s", "properties.PaddockdateIN":{"$date":"%s"}, "properties.PrevPaddock":"%s"}}',
                          paddock[i], pdkdf$`_id`, paste0(substr(date[i],1,10),"T","00:00:00","+1000"), cattledf$properties$Paddock)
        cattle$update(RFIDS, update)
        paste0("The Cattle collection has been updated for ", RFID[i], ".")
      }
    } else {
      paste(RFID[i], "is already allocated to", paddock[i], "in the Cattle collection.")
    }

    # -----------------------------------
    # PaddockHistory
    # -----------------------------------
    if(RFID[i] != "xxx xxxxxxxxxxxx"){
      RFIDS <- sprintf('{"RFID":"%s", "dateOUT":{"$exists":false}}', RFID[i])
    } else {
      RFIDS <- sprintf('{"stationname":"%s", "Management":"%s", "dateOUT":{"$exists":false}}', property, MTag[i])
    }
    pdkhistdf <- paddockhistory$find(query = RFIDS, fields = '{"_id":false, "RFID":true, "Management":true, "Paddock":true, "dateIN":true, "dateOUT":true, "currentPaddock":true}')

    if(pdkhistdf$Paddock != paddock[i]){
      if(as.Date(strftime(pdkhistdf$dateIN, format = "%Y-%m-%d")) > as.Date(date)[i]){
        stop(paste("The date allocated to", RFID[i], "is before the previous paddock date in the database."))
      } else {
        update <- sprintf('{"$set":{"dateOUT":{"$date":"%s"}, "currentPaddock":"%s"}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"), FALSE)
        paddockhistory$update(RFIDS, update)

        update <- sprintf('{"RFID":"%s", "cattle_id":"%s", "Management":"%s", "stationname":"%s", "Paddock":"%s", "currentPaddock":"%s", "dateIN":{"$date":"%s"}}',
                          RFID[i], cattledf$`_id`, MTag[i], property, paddock[i], TRUE, paste0(substr(date[i],1,10),"T00:00:00+1000"))
        paddockhistory$insert(update)
        paste0("The PaddockHistory collection has been updated for ", RFID[i], ".")
      }
    } else {
      paste(RFID[i], "is already allocated to", paddock[i], "in the PaddockHistory collection.")
    }

    # -----------------------------------
    # ALMS
    # -----------------------------------
    search <- sprintf('{"stationname":"%s", "properties.datarecording":"%s", "properties.type":"%s"}',
                      property, TRUE, "Walk-over-Weighing Unit")
    infsdf <- infs$find(query = search, fields = '{"_id":false, "properties.datarecording":true, "properties.asset_id":true, "properties.central":true}')

    if(infsdf$properties$central == "FALSE"){ # CENTRAL ALMS = FALSE
      if(RFID[i] != "xxx xxxxxxxxxxxx"){
        search <- sprintf('{"RFID":"%s", "currentALMS":"%s"}', RFID[i], TRUE)
      } else {
        search <- sprintf('{"stationname":"%s", "Management":"%s", "currentALMS":"%s"}', property, MTag[i], TRUE)
      }
      almshistdf <- almshistory$find(query = search)

      if(nrow(almshistdf) > 0){ # current paddock ALMS = TRUE
        update <- sprintf('{"$set":{"dateOFF":{"$date":"%s"}, "currentALMS":"%s"}}', paste0(substr(date[i],1,10),"T00:00:00+1000"), FALSE)
        almshistory$update(RFIDS, update)
      } else {} # current paddock ALMS = FALSE

      search <- sprintf('{"stationname":"%s", "properties.Paddock":"%s", "properties.datarecording":"%s", "properties.type":"%s"}',
                        property, paddock[i], TRUE, "Walk-over-Weighing Unit")
      infsdf <- infs$find(query = search, fields = '{"_id":false, "properties.datarecording":true, "properties.asset_id":true, "properties.central":true}')

      if(nrow(infsdf) > 0){ # future paddock ALMS = TRUE
        update <- sprintf('{"RFID":"%s", "cattle_id":"%s", "Management":"%s", "stationname":"%s", "ALMS":"%s", "currentALMS":"%s", "dateON":{"$date":"%s"}}',
                          RFID[i], cattledf$`_id`, MTag[i], property, infsdf$properties$asset_id, TRUE, paste0(substr(date[i],1,10),"T00:00:00+1000"))
        almshistory$insert(update)
        paste0("ALMSHistory has been updated for ", RFID[i], ".")
      } else {} # future paddock ALMS = FALSE
    } else {} # CENTRAL ALMS = TRUE
  }
}
