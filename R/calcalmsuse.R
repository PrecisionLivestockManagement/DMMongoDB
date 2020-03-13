#' Calculate cattle ALMS use from the DataMuster database
#'
#' This function calculates individual or groups of cattle ALMS use from the DataMuster database
#' @name calcalmsuse
#' @param property the name of the property
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the cattle numbers associated with the ALMS and the number of cattle recorded
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


calcalmsuse <- function(property, timezone, start=NULL, end=NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  inf <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  if(is.null(start)){start <- as.Date("2014-09-01")}
  if(is.null(end)){end <- Sys.time()
                   attr(end,"tzone") <- timezone
                   end <- as.Date(end, tz = timezone)}

  wowunits <- infsearch(property, infstype = "Walk-over-Weighing Unit", username = username, password = password)

  if(nrow(wowunits) == 0) {cattleinfo <- data.frame()}else{

  cows <- propsearchfull(property, archives = TRUE, username = username, password = password)

  cows <- cows%>%
          filter(RFID != "xxx xxxxxxxxxxxx")

  cattlehistory <- almshistsearch(property, start = start, end = end, timezone = timezone, username = username, password = password)

  if(length(cattlehistory$RFID) == 0){cattleinfo <- data.frame()}else{

  cattlehistory <- bind_rows(cattlehistory$ALMSHistory, .id = "RFID")%>%
    filter(RFID != "xxx xxxxxxxxxxxx")%>%
    mutate(dateOFF = as.character(dateOFF),
           dateOFF = ifelse(is.na(dateOFF), as.character(end), dateOFF))

  cattleweights <- get_dailywts(RFID = cows$RFID, start = start, end = end, timezone = timezone,
                                fields = c("RFID","datetime"),
                                username = username, password = password)

  cattleweights <- cattleweights%>%
    mutate(Date = as.Date(Date, tz = timezone))%>%
    select(RFID, Date)%>%
    distinct()

  usehistory <- data.frame()

  if (nrow(cattlehistory) == 0){}else{

    newstart <- min(cattlehistory$dateON)

    dates <- seq.Date(newstart, end, by = "days")

    for (k in 1:length(dates)){

      tempdf <- cattlehistory %>%
        filter(as.Date(dateON) <= dates[k] & as.Date(dateOFF) >= dates[k])%>%
        mutate(Date = dates[k])%>%
        select(Date, Property,ALMS, RFID)

      if (nrow(tempdf) == 0) {} else {
        usehistory <- rbind(usehistory, tempdf)
      }}

    usehistory$Count <- ifelse(paste0(usehistory$RFID, usehistory$Date) %in% paste0(cattleweights$RFID, cattleweights$Date), "1", "0")

    cattleinfo <- left_join(usehistory, cows, by = "RFID")%>%
      select(Date, Property, ALMS.x, RFID, Management, category, sex, Count)%>%
      rename(ALMS = "ALMS.x", Category = "category", Sex = "sex")
  }
  }
  }

  return(cattleinfo)

}
