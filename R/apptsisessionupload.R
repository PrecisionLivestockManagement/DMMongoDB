#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function allows Gallagher TSi session data to be uploaded from the DataMuster website
#' @name apptsisessionupload
#' @param Tag the RFID number as txt
#' @param EID the RFID number as txt
#' @param date the RFID number as txt
#' @param liveweight the RFID number as txt
#' @param property the RFID number as txt
#' @param NLIS the RFID number as txt
#' @param pdkfrom in correct format
#' @param pdkto in correct format
#' @param notes in correct format
#' @param username you will need to request access from Lauren O'Connor for a username to write data to the database
#' @param password you will need to request access from Lauren O'Connor for a username to write data to the database
#' @return uploads data in a dataframe to the DMMongoDB database
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
