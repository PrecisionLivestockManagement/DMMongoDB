#' Retrieve cattle pregnancy history from the DataMuster MongoDB database
#'
#' This function pulls in pregnancy history data for individual or groups of cattle. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_cattlepreghist
#' @param RFID a list of cattle RFID numbers
#' @param values the minimum number of calving date values required, default 1
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of RFID numbers and associated calving dates
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

get_cattlepreghist <- function(RFID,
                               values = NULL,
                               username = NULL,
                               password = NULL){

  if(is.null(values)|| values < 1 ){values <- 1}
  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "preghist.date":true, "preghist.foetalage":true, "_id":false}')


  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    if (length(jan2$preghist$date[i][[1]]) != 0){

    calving <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$preghist$date[[i]]), jan2$preghist$foetalage[[i]])),
                        c("Date", "foetalage"))
    calving$Date <- as.Date(jan2$preghist$date[[i]], tz = "Australia/Brisbane")

    #This is the section where we can apply further filters based on breed, class, etc.

    cattleinfo[[jan2$RFID[i]]] <- as.data.frame(calving)
    }else{
      jan2$RFID[[i]] <- "xxxx"}
}

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, CalvingDates=cattleinfo)

  return(cattleinfo)

}
