#' This function pulls in weekly weights for predefined cattle and predefined periods
#'
#' If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements
#' @name weeklywts
#' @param RFID this is a list of cattle RFID numbers
#' @param start provide a start date to be returned, this has to be in date format.
#' @param end provide a end date to be returned, this has to be in date format.
#' @return is a list that includes a list of the RFID numbers that have been searched, the list of cattle stations that are associated with these RFID numbers and then a dataframe for each RFID number #' that provides date and weekly average weight for each animal
#'@author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


weeklywts <- function(RFID, start=NULL, end=NULL){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wkwthist.date":true, "wkwthist.avweight":true, "_id":false}')

  cattleinfo <- list(RFID=jan2$RFID, Station=jan2$stationname)

  for(i in 1:length(jan2$RFID)){
    weeklywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$wkwthist$date[[i]]))), c("Date", "Weight"))
    weeklywts$Date <- jan2$wkwthist$date[[i]]
    weeklywts$Weight <- jan2$wkwthist$avweight[[i]]

    if(is.null(start)) {cattleinfo[[jan2$RFID[i]]] <- weeklywts}
    else{if(is.null(end)){weeklywts <- weeklywts %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{weeklywts <- weeklywts %>% filter(between(as.Date(Date),start,end))}}

    cattleinfo[[jan2$RFID[i]]] <- weeklywts


  }

  return(cattleinfo)

}
