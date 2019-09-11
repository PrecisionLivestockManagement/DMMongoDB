#' Remove cattle from a station and record cattle movement in NLIS database using RFID load ramp reader records
#'
#' This function removes individual or groups of cattle from a station. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name loadrampremove
#' @param RFID this is a list of cattle RFID numbers
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


loadrampremove <- function(RFID, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  loadramps <- mongo(collection = "LoadRamps", db = "DMIoT", url = pass, verbose = T)

   for (i in 1:length(RFID)){

     RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
     IDS <- sprintf('{"RFID":"%s", "actioned":"%s"}', RFID[i], "0")

    banger <- cattle$find(query = RFIDS, fields = '{"_id":false, "active":true, "stationname":true}')

    if (nrow(banger) == 1){
      if (banger$active == TRUE){
      trebble <- loadramps$find(query = IDS, fields = '{"_id":true, "datetime":true}')
      setdate <- as.Date(trebble$datetime[1], tz = "Australia/Brisbane")

     removecattle(RFID = RFID[i], property = , date = setdate, username = username, password = password)

    IDI <- sprintf('{"$set":{"actioned":"%s"}}', "1")

    for (k in 1:nrow(trebble)){
      IDS <- sprintf('{"_id":{"$oid":"%s"}}', trebble$`_id`[k])
      loadramps$update(IDS, IDI)}

    }}}
}



