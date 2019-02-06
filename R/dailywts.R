
#' @import dplyr
#' @export


dailywts <- function(RFID, start=NULL, end=NULL){


  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


  RFID <- paste(unlist(RFID), collapse = '", "' )
  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wthist.date":true, "wthist.weight":true, "_id":false}')

  cattleinfo <- list(RFID=jan$RFID, Station=jan$stationname)

  for(i in 1:length(jan$RFID)){
    dailywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan$wthist$date[[i]]))), c("Date", "Weight"))
    dailywts$Date <- jan$wthist$date[[i]]
    dailywts$Weight <- jan$wthist$weight[[i]]

    if(is.null(start)) {cattleinfo[[jan$RFID[i]]] <- dailywts}
    else{if(is.null(end)){dailywts <- dailywts %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{dailywts <- dailywts %>% filter(between(as.Date(Date),start,end))}}

    cattleinfo[[jan$RFID[i]]] <- dailywts


  }

  return(cattleinfo)


}




