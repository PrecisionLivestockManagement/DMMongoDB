#' Retrieve data on cattle allocations to ALMS units from the DataMuster database
#'
#' This function provides a list of cattle that have been allocated to ALMS units during specified time periods for a property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name almshistsearch
#' @param property the name of the property to query
#' @param alms a list of the ALMS asset_id's as character entries, if NULL all ALMS units for the property are loaded
#' @param start a start date to be returned in date format, default is "2015-01-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the cattle RFID numbers and dates on/off the ALMS unit/s
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


almshistsearch <- function(property=NULL, alms=NULL, start=NULL, end=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(start)){start <- as.Date("2015-01-01")}
  if(is.null(end)){end <- Sys.Date()}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  #property <- paste(unlist(property), collapse = '", "' )
  #filterstation1 <- sprintf('{"stationname":{"$in":["%s"]}}', property)
  #filterstation2 <- sprintf('{"$or": [{"exstation":"%s"}, {"stationname":"%s"}]}', property, property)

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "stationname":true, "exstation":true, "almshist.asset_id":true, "almshist.dateON":true, "almshist.dateOFF":true, "_id":false}')

  #jan2 <- cattle$find(query = filterstation2, fields=lookfor)
  jan2 <- cattle$find(query = '{}', fields=lookfor)

  if(is.null(property)){}else{
    jan2 <- jan2%>%
    filter(stationname %in% property | exstation %in% property)}

  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    dailywts <- setNames(data.frame(matrix(ncol = 4, nrow = length(jan2$almshist$asset_id[[i]]))), c("Property","ALMS", "dateON", "dateOFF"))

    if (nrow(dailywts) >= 1){
      dailywts$dateON <- as.Date(jan2$almshist$dateON[[i]], tz = "Australia/Brisbane")
      dailywts$ALMS <- jan2$almshist$asset_id[[i]]
      dailywts$Property <- ifelse(jan2$stationname[i] == "xxxxxx", jan2$exstation[i], jan2$stationname[i])
      if (length(jan2$almshist$dateON[[i]]) != 0){
        if (length(jan2$almshist$dateOFF[[i]]) != nrow(dailywts)){
          if(length(jan2$almshist$dateOFF[[i]]) == 0) {
            row <- NA}else{
            row <- c(as.Date(jan2$almshist$dateOFF[[i]], tz = "Australia/Brisbane"), NA)}
          dailywts$dateOFF <- row}else{
      dailywts$dateOFF <- as.Date(jan2$almshist$dateOFF[[i]], tz = "Australia/Brisbane")}}

    if(is.null(alms)){
      n <- c(1:length(dailywts$ALMS))}else{
      n <- which(dailywts$ALMS %in% alms)}

      if (length(n) != 0){

        for (p in 1:length(n)){

          if (is.na(dailywts$dateOFF[n[p]])){
          dates <- seq(as.Date(dailywts$dateON[n[p]]), Sys.Date(), by = "days")}else{
          dates <- seq(as.Date(dailywts$dateON[n[p]]), as.Date(dailywts$dateOFF[n[p]]), by = "days")}

          dates<-dates[between(dates, as.Date(start), as.Date(end))]

          if (length(dates) == 0){dailywts$ALMS[n[p]] <- NA}}}

      dailywts <- dailywts %>% filter(!(is.na(ALMS)))

      if(is.null(alms)){}else{
      dailywts <- dailywts %>% filter(!(is.na(ALMS)), ALMS %in% alms)}

      }


    #This is the section where we can apply further filters based on breed, class, etc.

    if (nrow(dailywts) != 0){
      cattleinfo[[jan2$RFID[i]]] <- as.data.frame(dailywts)}else{
        jan2$RFID[[i]] <- "xxxx"}
  }

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  #RFID$Property <- ifelse(RFID$stationname == "xxxxxx", RFID$exstation, RFID$stationname)
  cattleinfo <- list(RFID=RFID$RFID, Property=RFID$Property, ALMSHistory=cattleinfo)

  return(cattleinfo)

}


