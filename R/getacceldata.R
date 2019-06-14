#' Retrieve accelerometer data from the DataMuster MongoDB database
#'
#' This function pulls in daily weights for individual or groups of cattle for specified periods. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name getacceldata
#' @param start provide a start date and time to be returned, this has to be in datetime format.
#' @param end provide a end date and time to be returned, this has to be in datetime format.
#' @param sample returns a sample of data for the number of minutes specified. The default is to return all data.
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username.
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password.
#' @return a dataframe of x, y and z axis values in g-force (g).
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


getacceldata <- function(start=NULL, end=NULL, sample=NULL, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  acceldata <- mongo(collection = "AccelData", db = "PLMResearch", url = pass, verbose = T)

  jan2 <- acceldata$find(query = '{}', fields='{"DateTimeSecs":true, "xvalues":true, "yvalues":true,"zvalues":true, "_id":false}')

  return(jan2)

}




