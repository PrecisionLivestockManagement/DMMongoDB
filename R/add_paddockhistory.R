#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_paddockhistory
#' @param MTag a list of cattle management tag number/s
#' @param property the name of the property
#' @param calfMTag the calf's management tag number
#' @param birthDate the cow's date of calving in date format
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


add_paddockhistory <- function(RFID, cattle_id, MTag, property, Paddock, currentPaddock, dateIN, dateOUT = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    paddockhistory <- mongo(collection = "PaddockHistory", db = "DataMuster", url = pass, verbose = T)

    if(length(currentPaddock) != length(RFID)){currentPaddock <- rep(currentPaddock, length(RFID))}
    if(length(property) != length(RFID)){property <- rep(property, length(RFID))}
    if(length(dateIN) != length(RFID)){dateIN <- rep(dateIN, length(RFID))}
    if(length(Paddock) != length(RFID)){Paddock <- rep(Paddock, length(RFID))}
    if(!(is.null(dateOUT)) && length(dateOUT) != length(RFID)){dateOUT <- rep(dateOUT, length(RFID))}

    if(!(is.null(dateOUT))){

      data <- sprintf('{"RFID":"%s", "cattle_id":"%s", "Management":"%s", "stationname":"%s", "Paddock":"%s", "currentPaddock":"%s", "dateIN":{"$date":"%s"}, "dateOUT":{"$date":"%s"}}',
                          RFID, cattle_id, MTag, property, Paddock, currentPaddock, paste0(substr(dateIN,1,10),"T00:00:00+1000"), paste0(substr(dateOUT,1,10),"T00:00:00+1000"))
    }else{

      data <- sprintf('{"RFID":"%s", "cattle_id":"%s", "Management":"%s", "stationname":"%s", "Paddock":"%s", "currentPaddock":"%s", "dateIN":{"$date":"%s"}}',
                      RFID, cattle_id, MTag, property, Paddock, currentPaddock, paste0(substr(dateIN,1,10),"T00:00:00+1000"))
    }

    paddockhistory$insert(data)

    }





