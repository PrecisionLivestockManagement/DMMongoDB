#' Add static weight data to the DataMuster MongoDB database.
#'
#' This function adds static weight data for individual or groups of cattle in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_staticwts
#' @param RFID a list of cattle RFID number/s
#' @param weight a list of the cattle weights
#' @param date the date that the weight was measured in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the static weights have been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


update_stweight <- function(RFID, weight, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  staticwts <- mongo(collection = "StaticWts", db = "DataMuster", url = pass, verbose = T)

  if(is.null(date)){date <- Sys.Date()}else{date <- as.Date(date)}

  if (length(date) == 1){date <- rep(date, length = length(RFID))}


  for (i in 1:length(RFID)){

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

    dat <- date[i]
    wt <- weight[i]

    wt <- ifelse(wt == "" | is.na(wt), 0, wt)

    if(!(is.na(dat))){

        cattleinfo <- cattle$find(query = RFIDS, fields='{"_id":true, "stationname":true}')

        matchwt <- staticwts$find(query = sprintf('{"cattle_id":"%s"}', cattleinfo$`_id`), fields='{"RFID":true, "Date":true, "Weight":true, "_id":false}')

        if (nrow(matchwt) != 0){
        matchdate <- which(as.Date(matchwt$Date, tz = "Australia/Brisbane") == dat)
        matchwt <- matchwt$Weight[matchdate]
        }

        if (exists("matchdate") && wt %in% matchwt){}else{

          RFIDI <- sprintf('{"$set":{"properties.stweight":%s, "properties.stwtdate":{"$date":"%s"}}}', wt, paste0(substr(dat,1,10),"T","00:00:00","+1000"))

          data <- sprintf('{"RFID":"%s", "Date":{"$date":"%s"}, "Weight":%s, "stationname":"%s", "cattle_id":"%s"}',
                          RFID[i], paste0(substr(dat,1,10),"T00:00:00+1000"), as.numeric(wt), cattleinfo$stationname, cattleinfo$`_id`)

      cattle$update(RFIDS, RFIDI)
      staticwts$insert(data)

      }}}

}


