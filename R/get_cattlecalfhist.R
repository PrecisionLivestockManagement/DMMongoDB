#' Retrieve cattle calf history information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve cattle calf history from the Cattle collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_cattlecalfhist
#' @param RFID a list of cattle RFID number/s
#' @param values the minimum number of values required, default is 1
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated cattle calf history
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

get_cattlecalfhist <- function(RFID, values = NULL, username = NULL, password = NULL){

  if(is.null(values)|| values < 1 ){values <- 1}
  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "calfhist.date":true, "calfhist.ID":true}')


  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    if (length(jan2$calfhist$date[i][[1]]) != 0){

    calving <- setNames(data.frame(matrix(ncol = 2,
                                          nrow = length(jan2$calfhist$date[[i]]),
                                          jan2$calfhist$ID[[i]])),
                        c("date", "calfstatus"))
    calving$date <- as.Date(jan2$calfhist$date[[i]], tz = "Australia/Brisbane")
    calving$calfstatus <- as.character(calving$calfstatus)

    #This is the section where we can apply further filters based on breed, class, etc.

    cattleinfo[[jan2$RFID[i]]] <- as.data.frame(calving)
    }else{
      jan2$RFID[[i]] <- "xxxx"}
}

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, calfhist=cattleinfo)

  return(cattleinfo)

}
