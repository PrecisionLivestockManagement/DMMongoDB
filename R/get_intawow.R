#' Retrieve WoW information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve WoW information from the INTAWoW collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @param start a start date and time to be returned in datetime format
#' @param end an end date and time to be returned in datetime format
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username.
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password.
#' @return a dataframe of cattle RFID numbers and associated WoW information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


get_intawow <- function(start=NULL, end=NULL, username, password){

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "INTAWoW", db = "DMIoT", url = pass, verbose = T)

  data <- wowdata$find(query = '{}', fields='{"RFID":true, "Wt":true, "datetime":true,"Location":true, "Temp":true, "_id":false}')

  data <- data%>%
    mutate(datetime = as.POSIXct(format(datetime, tz="America/Argentina/Buenos_Aires",usetz=TRUE)))

  if(is.null(start)) {}
  else{if(is.null(end)){data <- data %>% filter(between(as.Date(datetime, tz = "America/Argentina/Buenos_Aires"),start,Sys.Date()))}
    else{data <- data %>% filter(between(as.Date(datetime, tz = "America/Argentina/Buenos_Aires"),start,end))}}

  data <- data%>%
    mutate(datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Argentina/Buenos_Aires")))%>%
   rename(Weight = "Wt", Datetime = "datetime", Temperature = "Temp")

  return(data)

}




