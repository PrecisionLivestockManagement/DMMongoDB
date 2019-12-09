#' Retrieve data from the DataMuster MongoDB database
#'
#' This function pulls in WoW weights for specified time periods. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name wowdatasearch
#' @param start provide a start date and time to be returned, in date format.
#' @param end provide a end date and time to be returned, in date format.
#' @param location the name of the alms unit, if NULL data for all units will be returned
#' @param remove.telemetry if TRUE the telemetry signals will be removed from the search results, if null or FALSE the telemetry signal data will be included in the search results
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password
#' @return a dataframe of WoW weights that have been recorded during specified time periods
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


wowdatasearch <- function(start=NULL, end=NULL, location=NULL, remove.telemetry=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "WoWData", db = "DMIoT", url = pass, verbose = T)

  data <- wowdata$find(query = '{}', fields='{"RFID":true, "Wt":true, "datetime":true, "Location":true, "import":true, "match":true, "_id":false}')

  #data <- data%>%
    #mutate(datetime = as.POSIXct(format(datetime, tz="America/Argentina/Buenos_Aires",usetz=TRUE)))

  if(is.null(start)){}else{
    if(is.null(end)){data <- data %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,Sys.Date()))}
    else{data <- data %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"),start,end))}}

  data <- data%>%
    mutate(datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")))%>%
    rename(Weight = "Wt", Datetime = "datetime")%>%
    select("RFID", "Weight", "Datetime", "Location")

  if(is.null(location)){}else{
    data <- data%>%
      filter(Location %in% location)
  }

  if(is.null(remove.telemetry)){}else{
    if(remove.telemetry == "TRUE"){
    data <- data%>%
      filter(RFID != "942 _telemetry")
  }}

  return(data)

}




