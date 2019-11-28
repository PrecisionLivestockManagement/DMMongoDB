#' Retrieve data on cattle ALMS daily weights from the DataMuster database
#'
#' This function provides a list of daily weights for individual or groups of cattle for specified periods. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name dailywtsNEW
#' @param RFID a list of cattle RFID numbers
#' @param start a start date to be returned in date format.
#' @param end an end date to be returned in date format
#' @param values the minimum number of weight values required, default is 1
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated daily weights and dates. Please note daily ALMS records are not reliable indicators of cattle weights, the weekly average data is much more reliable. However, the daily values can be useful to identify fine scale changes in weight associated with calving events. The daily data is also available for researchers to identify improved weekly averaging algorithms based on the daily data.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


dailywtsNEW <- function(RFID, start=NULL, end=NULL, values=NULL, username=NULL, password=NULL){


  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(values)||values < 1 ){values <- 1}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  dailywts <- mongo(collection = "DailyWts", db = "DataMuster", url = pass, verbose = T)

  RFID <- paste(unlist(RFID), collapse = '", "' )
  filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  lookfor <- sprintf('{"RFID":true, "stationname":true, "properties.Paddock":true, "RFIDhist.ID":true, "_id":false}')

  cows <- cattle$find(query = filtercattle, fields= lookfor)

  cattleinfo <- list()

  for(i in 1:length(cows$RFID)){

  rfids <- c(unique(cows$RFID[i], unlist(cows$RFIDhist$ID[i])))
  rfids <- paste(unlist(rfids), collapse = '", "' )

  filterwts <- sprintf('{"RFID":{"$in":["%s"]}}', rfids)

  wts <- dailywts$find(query = filterwts, fields= '{"RFID":true, "Wt":true, "datetime":true, "_id":false}')

  if(is.null(start)){}else{if(is.null(end)){wts <- wts %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,Sys.Date()))}
      else{wts <- wts %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,end))}}

  cattlewts <- setNames(data.frame(matrix(ncol = 2, nrow = nrow(wts))), c("Date", "Weight"))

   if(nrow(cattlewts)<values){cows$RFID[i] <- "xxxx"}else{

    cattlewts$Date <- as.POSIXct(strptime(wts$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
    cattlewts$Weight <- wts$Wt

    cattlewts <- cattlewts[order(cattlewts$Date),]

    cattleinfo[[cows$RFID[i]]] <- as.data.frame(cattlewts)}
  }

  RFID <- cows[which(cows$RFID!="xxxx"),]
  cattleinfo <- list(RFID=RFID$RFID, Property=RFID$stationname, Paddock=RFID$properties$Paddock, DailyWeights=cattleinfo)

  return(cattleinfo)

}




