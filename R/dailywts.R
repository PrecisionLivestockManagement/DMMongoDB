#' Cattle daily weights
#'
#' This function pulls in daily weights for individual or groups of cattle for specified periods. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name dailywts
#' @param RFID this is a list of cattle RFID numbers
#' @param start provide a start date to be returned, this has to be in date format.
#' @param end provide a end date to be returned, this has to be in date format.
#' @param values this is the minimum number of weight values that need to be recorded to be uploaded. The default is to load all animals that have at least one value.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return A list that includes a list of the RFID numbers that have been returned, the list of cattle stations that are associated with these RFID numbers and a dataframe for each RFID number that provides date and daily weights for each animal. Please note daily ALMS records are not reliable indicators of cattle weights, the weekly average data is much more reliable. However, the daily values can be useful to identify fine scale changes in weight associated with calving events. The daily data is also available for researchers to identify improved weekly averaging algorithms based on the daily data.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


dailywts <- function(RFID, start=NULL, end=NULL, values=NULL, username=NULL, password=NULL){

  if(is.null(values)||values < 1 ){values <- 1}
  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}
  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wthist.date":true, "wthist.weight":true, "_id":false}')


  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){

    dailywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$wthist$date[[i]]))), c("Date", "Weight"))
    dailywts$Date <- jan2$wthist$date[[i]]
    dailywts$Weight <- jan2$wthist$weight[[i]]

    if(is.null(start)) {}
    else{if(is.null(end)){dailywts <- dailywts %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{dailywts <- dailywts %>% filter(between(as.Date(as.POSIXct(Date, tz = "Australia/Brisbane"),tz = "Australia/Brisbane"),start,end))}}

    #This is the section where we can apply further filters based on breed, class, etc.

    if(nrow(dailywts)<values){jan2$RFID[[i]] <- "xxxx"}
    else{cattleinfo[[jan2$RFID[i]]] <- as.data.frame(dailywts)}
  }

  RFID <- jan2[which(jan2$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, Property=RFID$stationname, DailyWeights=cattleinfo)

  return(cattleinfo)



}




