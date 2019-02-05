
#' @import dplyr
#' @export


dailywts <- function(RFID, start, end){


  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB")

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)


  filterstation <- sprintf('{"RFID":"%s"}', RFID)
  jan <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wthist.date":true, "wthist.weight":true, "_id":false}')

  dailywts <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan$wthist$date[[1]]))), c("Date", "Weight"))

  dailywts$Date <- jan$wthist$date[[1]]
  dailywts$Weight <- jan$wthist$weight[[1]]

  #return(dailywts)

  dailywts <- dailywts %>% filter(between(as.Date(Date),start,end))

  return(list(RFID=jan$RFID, Station=jan$stationname, Wts=dailywts))

}



