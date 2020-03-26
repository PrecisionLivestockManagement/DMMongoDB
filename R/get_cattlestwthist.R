#' Retrieve cattle static weight history from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve cattle static weight history from the Cattle collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_cattlestwthist
#' @param RFID this is a list of cattle RFID numbers
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param values the minimum number of values required, default is 1
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated static weight
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

get_cattlestwthist <- function(RFID, start=NULL, end=NULL, values=NULL, username = NULL, password = NULL){

  if(is.null(values)||values < 1 ){values <- 1}
  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "stwthist.date":true, "stwthist.weight":true, "_id":false}')


  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    dailywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$stwthist$date[[i]]))), c("Date", "Weight"))

    if (nrow(dailywts) >= 1) {
    dailywts$Date <- as.Date(jan2$stwthist$date[[i]], tz = "Australia/Brisbane")
    dailywts$Weight <- jan2$stwthist$weight[[i]]}

    if(is.null(start)) {}
    else{if(is.null(end)){dailywts <- dailywts %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{dailywts <- dailywts %>% filter(between(as.Date(Date),start,end))}}

    #This is the section where we can apply further filters based on breed, class, etc.

    if(nrow(dailywts)<values){jan2$RFID[[i]] <- "xxxx"}
    else{cattleinfo[[jan2$RFID[i]]] <- as.data.frame(dailywts)}
  }

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, Property=RFID$stationname, DailyWeights=cattleinfo)

  return(cattleinfo)

}
