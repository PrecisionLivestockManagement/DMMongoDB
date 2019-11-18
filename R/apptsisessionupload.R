#' Upload Gallagher TSi session data to the DataMuster database
#'
#' This function allows Gallagher TSi session data to be uploaded via the DataMuster website
#' @name apptsisessionupload
#' @param Tag a list of cattle management tag numbers
#' @param EID a list of cattle RFID numbers
#' @param date a list of dates in date format
#' @param liveweight a list of cattle weights
#' @param property the name of the property
#' @param NLIS a list of cattle RFID numbers
#' @param pdkfrom a list of paddock names the cattle have moved from
#' @param pdkto a list of paddock names the cattle have moved to
#' @param notes observations made by the stationhand as a character entry
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully uploaded
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @export


apptsisessionupload <- function(Tag, EID, date, liveweight, property, NLIS=NULL, pdkfrom=NULL, pdkto=NULL, notes=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  tsidata <- mongo(collection = "apptsi", db = "DMIoT", url = pass, verbose = T)

  date <- as.Date(date, format = "%d/%m/%Y")
  liveweight <- ifelse(liveweight == "", 0, liveweight)
  NLIS <- ifelse(is.null(NLIS), rep("",length(EID)), NLIS)
  pdkfrom <- ifelse(is.null(pdkfrom), rep("",length(EID)), pdkfrom)
  pdkto <- ifelse(is.null(pdkto), rep("",length(EID)), pdkto)
  notes <- ifelse(is.null(notes), rep("",length(EID)), notes)

  data <- sprintf('{"Tag Number":"%s", "Electronic ID":"%s", "Last Seen Date":{"$date":"%s"}, "Live Weight (kg)":%s, "NLIS":"%s", "Pdk from":"%s", "Pdk to":"%s", "Notes":"%s", "stationname":"%s", "createdAt":{"$date":"%s"}}',
                  Tag, EID, paste0(substr(date,1,10),"T","00:00:00","+1000"), liveweight, NLIS, pdkfrom, pdkto, notes, property, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  tsidata$insert(data)

}
