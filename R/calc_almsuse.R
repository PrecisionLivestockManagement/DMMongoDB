#' Calculate cattle ALMS use from the DataMuster MongoDB database.
#'
#' This function calculates individual or groups of cattle ALMS use using the DataMuster Mongo DB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name calc_almsuse
#' @param property the name of the property
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the cattle RFID numbers and ALMS use statistics
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


calc_almsuse <- function(property, timezone, start=NULL, end=NULL, username = NULL, password = NULL){

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

  wowunits <- get_infrastructure(property = property, type = "Walk-over-Weighing Unit",
                                 fields = c("properties.asset_id", "stationname", "_id"),
                                 username = username, password = password)

  if(nrow(wowunits) == 0) {cattleinfo <- data.frame()}else{

  df <- get_cattle(property = property, fields = c("RFID", "properties.Management", "properties.category", "properties.sex", "stationname"), username = username, password = password)
  df <- df %>% rename("Property" = stationname)

  dt <- get_cattle(exstation = property, fields = c("RFID", "properties.Management", "properties.category", "properties.sex", "exstation"), username = username, password = password)
  dt <- dt %>% rename("Property" = exstation)

  cows <- rbind(df, dt)

  cows <- cows%>%
          filter(RFID != "xxx xxxxxxxxxxxx")

  if(nrow(cows) == 0){cattleinfo <- data.frame()}else{

    cattlehistory <- get_almshistory(RFID = cows$RFID, fields = c("RFID", "ALMS", "dateON", "dateOFF"),
                                      username = username, password = password)

  # cattlehistory <- get_cattlealmshist(RFID = cows$RFID, username = username, password = password)
  #
   if(length(cattlehistory$RFID) == 0){cattleinfo <- data.frame()}else{
  #   cattlehistory <- bind_rows(cattlehistory$ALMShist, .id = "RFID") %>%
  #     filter(RFID != "xxx xxxxxxxxxxxx") %>%
  #     mutate(dateOFF = as.character(dateOFF),
  #            dateOFF = ifelse(is.na(dateOFF), as.character(end), dateOFF)) %>%
  #     filter(dateON >= start) %>%
  #     filter(dateOFF <= end)

    cattlehistory <- cattlehistory %>%
                      filter(RFID != "xxx xxxxxxxxxxxx") %>%
                      mutate(dateON = as.Date(dateON, tz = timezone),
                             dateOFF = as.character(dateOFF),
                             dateOFF = ifelse(is.na(dateOFF), as.character(end), dateOFF)) %>%
                      filter(dateON >= start) %>%
                      filter(dateOFF <= end)

    #cattlehistory$dateOFF <- as.character(cattlehistory$dateOFF)
    #cattlehistory$dateOFF <- ifelse(is.na(cattlehistory$dateOFF), as.character(end), cattlehistory$dateOFF)

  cattleweights <- get_dailywts(RFID = cows$RFID, start = start, end = end, timezone = timezone,
                                fields = c("RFID","datetime"),
                                username = username, password = password)

  cattleweights <- cattleweights%>%
    mutate(Date = as.Date(Date, tz = timezone))%>%
    select(RFID, Date)%>%
    distinct()

  usehistory <- data.frame()

  if (nrow(cattlehistory) == 0){cattleinfo <- data.frame()}else{

    newstart <- min(cattlehistory$dateON)

    dates <- seq.Date(newstart, end, by = "days")

    for (k in 1:length(dates)){

      tempdf <- cattlehistory %>%
        filter(as.Date(dateON) <= dates[k] & as.Date(dateOFF) >= dates[k])%>%
        mutate(Date = dates[k])%>%
        select(Date, ALMS, RFID)

      if (nrow(tempdf) == 0) {} else {
        usehistory <- rbind(usehistory, tempdf)
      }}

    usehistory$Count <- ifelse(paste0(usehistory$RFID, usehistory$Date) %in% paste0(cattleweights$RFID, cattleweights$Date), "1", "0")

    cattleinfo <- left_join(usehistory, cows, by = "RFID") %>%
      select(Date, Property, ALMS, RFID, Management, category, sex, Count) %>%
      rename(Category = "category", Sex = "sex")
}
  }
  }}

  return(cattleinfo)

}
