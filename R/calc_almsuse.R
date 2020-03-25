#' Calculate cattle ALMS use from the DataMuster database
#'
#' This function calculates individual or groups of cattle ALMS use from the DataMuster database
#' @name calc_almsuse
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
                                 fields = c("properties.asset_id", "properties.type", "properties.usenum",
                                            "properties.datarecording", "properties.filename",
                                            "properties.Paddock", "properties.telemetry_out",
                                            "properties.lastsignal", "properties.statEID", "properties.statweight",
                                            "properties.statdate", "stationname", "_id"))
  # wowunits <- infsearch(property, infstype = "Walk-over-Weighing Unit", username = username, password = password)

  if(nrow(wowunits) == 0) {cattleinfo <- data.frame()}else{

  df <- get_cattle(property = property,
                   fields = c("properties.birthDate", "properties.deathDate", "properties.sex",
                              "properties.breed", "properties.recordedtime", "properties.Management",
                              "properties.Paddock", "properties.category", "properties.horn",
                              "properties.colour", "properties.stweight", "properties.stwtdate",
                              "properties.birthWeight", "properties.damRFID", "properties.sireRFID",
                              "properties.ALMS", "properties.ALMSasset_id", "properties.wkweight",
                              "properties.wkwtdate", "properties.brand", "properties.damMTag",
                              "properties.weaned", "properties.exitDate", "properties.entryDate",
                              "properties.calvingdate", "properties.PaddockdateIN", "RFID"))
  dt <- get_cattle(exstation = property,
                   fields = c("properties.birthDate", "properties.deathDate", "properties.sex",
                              "properties.breed", "properties.recordedtime", "properties.Management",
                              "properties.Paddock", "properties.category", "properties.horn",
                              "properties.colour", "properties.stweight", "properties.stwtdate",
                              "properties.birthWeight", "properties.damRFID", "properties.sireRFID",
                              "properties.ALMS", "properties.ALMSasset_id", "properties.wkweight",
                              "properties.wkwtdate", "properties.brand", "properties.damMTag",
                              "properties.weaned", "properties.exitDate", "properties.entryDate",
                              "properties.calvingdate", "properties.PaddockdateIN", "RFID"))
  cows <- rbind(df, dt)

  cows <- cows%>%
          filter(RFID != "xxx xxxxxxxxxxxx")

  cattlehistory <- get_cattlealmshist(RFID = cows$RFID, username = username, password = password)

  if(length(cattlehistory$RFID) == 0){cattleinfo <- data.frame()}else{
    cattlehistory <- bind_rows(cattlehistory$ALMShist, .id = "RFID") %>%
      filter(RFID != "xxx xxxxxxxxxxxx") %>%
      filter(dateON >= start) %>%
      filter(dateOFF <= end)
    cattlehistory$dateOFF <- as.character(cattlehistory$dateOFF)
    cattlehistory$dateOFF <- ifelse(is.na(cattlehistory$dateOFF), as.character(end), cattlehistory$dateOFF)

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
        select(Date, asset_id, RFID)

      if (nrow(tempdf) == 0) {} else {
        usehistory <- rbind(usehistory, tempdf)
      }}

    usehistory$Count <- ifelse(paste0(usehistory$RFID, usehistory$Date) %in% paste0(cattleweights$RFID, cattleweights$Date), "1", "0")

    cattleinfo <- left_join(usehistory, cows, by = "RFID") %>%
      select(Date, asset_id, RFID, Management, category, sex, Count) %>%
      rename(ALMS = "asset_id", Category = "category", Sex = "sex")

  }
  }
  }

  return(cattleinfo)

}