#' Retrieve UHF data from the DataMuster MongoDB database
#'
#' This function pulls in data for specified time periods. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name getuhfdata
#' @param start provide a start date and time to be returned, this has to be in datetime format.
#' @param end provide a end date and time to be returned, this has to be in datetime format.
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username.
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password.
#' @return a dataframe of UHF data that has been recorded during specified time periods.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


getuhfdata <- function(start=NULL, end=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  uhfdata <- mongo(collection = "UHFData", db = "DMIoT", url = pass, verbose = T)

  data <- uhfdata$find(query = '{}', fields='{"RFID":true, "datetime":true, "Wt":true, "_id":false}')

  data <- data%>%
   mutate(datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")))%>%
    arrange(datetime)%>%
    rename("Reader" = Wt, "Datetime" = datetime)

  if(is.null(start)) {}
  else{if(is.null(end)){data <- data %>% filter(between(as.Date(Datetime, tz = "Australia/Brisbane"),start,Sys.Date()))}
    else{data <- data %>% filter(between(as.Date(Datetime, tz = "Australia/Brisbane"),start,end))}}

  return(data)

}




