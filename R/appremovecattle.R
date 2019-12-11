#' Remove cattle from a station via the DataMuster website app
#'
#' This function removes individual or groups of cattle from a station. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name appremovecattle
#' @param RFID this is a list of cattle RFID numbers
#' @param MTag this is a list of cattle management tag numbers
#' @param property this is the name of the station
#' @param date provide the date that the animal left the station, this has to be in date format. Default is today's date.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


appremovecattle <- function(RFID, MTag, property, date, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
    }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  for (i in 1:length(RFID)){

    if(RFID[i] != "xxx xxxxxxxxxxxx"){
    IDS <- sprintf('{"RFID":"%s"}', RFID[i])}else{
    IDS <- sprintf('{"stationname":"%s", "properties.Management":"%s"}', property, MTag[i])}

    banger <- cattle$find(query = IDS, fields ='{"pdkhist.dateOUT":true, "_id":false}')
    arrpos <- length(banger$pdkhist$dateOUT[[1]])

    IDI <- sprintf('{"$set":{"stationname":"%s", "stationID":"%s", "active":"%s", "exstation":"%s", "geometry.coordinates.0":%s, "geometry.coordinates.1":%s, "properties.Paddock":"%s",
                   "properties.PaddockID":"%s", "properties.exitDate":{"$date":"%s"}, "properties.ALMS":"%s", "properties.ALMSID":"%s", "properties.ALMSasset_id":"%s"}}',
                   "xxxxxx", "xxxxxx", "FALSE", property, 0.0, 0.0, "xxxxxx", "xxxxxx", paste0(date,"T","00:00:00","+1000"), "FALSE", "xxxxxx", "xxxxxx")

    IDL <- sprintf('{"$set":{"pdkhist.dateOUT.%s":{"$date":"%s"}}}', arrpos, paste0(date,"T","00:00:00","+1000"))

    cattle$update(IDS, IDL) # Have to do this one first before stationname changes to "xxxxxx"
    cattle$update(IDS, IDI)

    }


}



