#' Retrieve cattle paddock history information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve cattle paddock history from the Cattle collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_cattlepdkhist
#' @param property the name of the property to search for
#' @param paddock the name of the paddock/s to search for. If not value is entered, then all paddocks are returned
#' @param archives if TRUE, the list of animals returned will include those that are no longer on the property
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated cattle paddock history
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


get_cattlepdkhist <- function(property, paddock=NULL, archives=NULL, start=NULL, end=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(start)){start <- as.Date("2014-01-01")}
  if(is.null(end)){end <- Sys.Date()}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
  #filterstation1 <- sprintf('{"stationname":{"$in":["%s"]}}', property)
  filterstation2 <- sprintf('{"$or": [{"exstation":"%s"}, {"stationname":"%s"}]}', property, property)

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "stationname":true, "pdkhist.name":true, "pdkhist.dateIN":true, "pdkhist.dateOUT":true, "_id":false}')

    jan2 <- cattle$find(query = filterstation2, fields=lookfor)

  cattleinfo <- list()

  for(i in 1:length(jan2$RFID)){
    pdk <- setNames(data.frame(matrix(ncol = 3, nrow = length(jan2$pdkhist$name[[i]]))), c("name", "dateIN", "dateOUT"))

    if (nrow(pdk) >= 1){
      pdk$dateIN <- as.Date(jan2$pdkhist$dateIN[[i]], tz = "Australia/Brisbane")
      pdk$name <- jan2$pdkhist$name[[i]]
      if (length(jan2$pdkhist$dateOUT[[i]]) != 0){
        if (length(jan2$pdkhist$dateOUT[[i]]) != nrow(pdk)){
          if (length(jan2$pdkhist$dateOUT[[i]]) == 0) {
            row <- NA}else{
          row <- c(as.Date(jan2$pdkhist$dateOUT[[i]], tz = "Australia/Brisbane"), NA)}
          pdk$dateOUT <- row}else{
      pdk$dateOUT <- as.Date(jan2$pdkhist$dateOUT[[i]], tz = "Australia/Brisbane")}}}

    if(is.null(paddock)){

      if (nrow(pdk) != 0){

        for (p in 1:nrow(pdk)){
          if (is.na(as.Date(pdk$dateOUT[p]))){
            dates <- seq(as.Date(pdk$dateIN[p]), Sys.Date(), by = "days")
            dates<-dates[between(dates, as.Date(start), as.Date(end))]
          }else{
            dates <- seq(as.Date(pdk$dateIN[p]), as.Date(pdk$dateOUT[p]), by = "days")
            dates<-dates[between(dates, as.Date(start), as.Date(end))]}

          if (length(dates) == 0){pdk$name[p] <- ""}}

      pdk <- pdk %>% filter(name != "")}

    }else{

    n <- which(pdk$name == paddock)

    if (length(n) != 0){

    for (p in 1:length(n)){
      if (is.na(as.Date(pdk$dateOUT[n[p]]))){
        dates <- seq(as.Date(pdk$dateIN[n[p]]), Sys.Date(), by = "days")
        dates<-dates[between(dates, as.Date(start), as.Date(end))]
      }else{
      dates <- seq(as.Date(pdk$dateIN[n[p]]), as.Date(pdk$dateOUT[n[p]]), by = "days")
      dates<-dates[between(dates, as.Date(start), as.Date(end))]}

      if (length(dates) == 0){pdk$name[n[p]] <- ""}}}

      pdk <- pdk %>% filter(paddock %in% name)
      }

    #This is the section where we can apply further filters based on breed, class, etc.

    if (nrow(pdk) != 0){
      cattleinfo[[jan2$properties$Management[i]]] <- as.data.frame(pdk)}else{
        jan2$properties$Management[[i]] <- "xxxx"}
  }

  RFID <- jan2[which(jan2$properties$Management!="xxxx"),]
  cattleinfo <- list(Management=RFID$properties$Management, RFID=RFID$RFID, PaddockHistory=cattleinfo)

  return(cattleinfo)

}


