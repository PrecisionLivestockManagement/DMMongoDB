#' Retrieve WoW information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve WoW information from the WoWData collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_wowdata
#' @param RFID a list of cattle RFID number/s
#' @param property the name of the property to search for
#' @param start a start date and time to be returned in datetime format, default is “2014-09-01 00:00:00”
#' @param end an end date and time to be returned in datetime format, default is today’s date and time
#' @param timezone the timezone, default is "Australia/Brisbane"
#' @param coord whether ALMS coordinates are returned. TRUE or FALSE, default is FALSE
#' @param username required for access. Please email \email{info@@datamuster.net.au} to acquire a username.
#' @param password required for access. Please email \email{info@@datamuster.net.au} to acquire a password.
#' @return a dataframe of cattle RFID numbers and associated WoW information
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


get_wowdata <- function(RFID = NULL, property = NULL, minwt = NULL, start=NULL, end=NULL, timezone = NULL, coord = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  wowdata <- mongo(collection = "WoWData", db = "DMIoT", url = pass, verbose = T)

  # data <- wowdata$find(query = '{}', fields='{"RFID":true, "Wt":true, "datetime":true,"Location":true, "_id":false}')

  #data <- data%>%
  #  mutate(datetime = as.POSIXct(format(datetime, tz="America/Argentina/Buenos_Aires",usetz=TRUE)))

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
  RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(property)){}else{property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"Location":{"$in":["%s"]},', property)}

  if(is.null(timezone)){timezone = "Australia/Brisbane"}

  if(is.null(start)){}else{
    if(timezone == "Australia/Brisbane"){
      start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    }
    if(timezone == "America/Argentina/Buenos_Aires"){
      start <- sprintf('"datetime":{"$gte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(start, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    }
  }

  if(is.null(end)){}else{
    if(timezone == "Australia/Brisbane"){
      end <- sprintf('"datetime":{"$lte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end, "00:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    }
    if(timezone == "America/Argentina/Buenos_Aires"){
      end <- sprintf('"datetime":{"$lte":{"$date":"%s"}},', strftime(as.POSIXct(paste0(end, "13:00:00")), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    }
  }

  if(is.null(coord)){coord <- FALSE}

  # Set up find query
  search <-paste0("{", RFID, property, start, end,"}")

  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

  # Query database and format for website display

  data <- wowdata$find(query = search)

  if(coord == TRUE){} else {
    data <- data %>%
      filter(!grepl("\\D", RFID) == TRUE,
             nchar(RFID) == 15)
  }

  # data <- data%>%
  #   mutate(datetime = as.POSIXct(strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Argentina/Buenos_Aires")))%>%
  #  rename(Weight = "Wt", Datetime = "datetime")

  return(data)

}




