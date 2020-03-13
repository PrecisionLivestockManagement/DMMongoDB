#' Retrieve cattle paddock history from the DataMuster MongoDB database
#'
#' This function provides a list of cattle paddock history
#' for a property from DataMuster MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name get_cattlepdkhist
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param paddock this is the name of a paddock or list of paddocks as character entries, if no value is entered then all paddocks are loaded
#' @param archives if true, the list of animals returned will include those that are no longer on the property
#' @param start provide a start date to be returned, this has to be in date format.
#' @param end provide a end date to be returned, this has to be in date format.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers and paddock history
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


get_cattlepdkhist <- function(property, paddock=NULL, achives=NULL, start=NULL, end=NULL, username=NULL, password=NULL){

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

    dailywts <- setNames(data.frame(matrix(ncol = 3, nrow = length(jan2$pdkhist$name[[i]]))), c("name", "dateIN", "dateOUT"))

    if (nrow(dailywts) >= 1){
      dailywts$dateIN <- as.Date(jan2$pdkhist$dateIN[[i]], tz = "Australia/Brisbane")
      dailywts$name <- jan2$pdkhist$name[[i]]
      if (length(jan2$pdkhist$dateOUT[[i]]) != 0){
        if (length(jan2$pdkhist$dateOUT[[i]]) != nrow(dailywts)){
          if (length(jan2$pdkhist$dateOUT[[i]]) == 0) {
            row <- NA}else{
          row <- c(as.Date(jan2$pdkhist$dateOUT[[i]], tz = "Australia/Brisbane"), NA)}
          dailywts$dateOUT <- row}else{
      dailywts$dateOUT <- as.Date(jan2$pdkhist$dateOUT[[i]], tz = "Australia/Brisbane")}}}

    if(is.null(paddock)){

      if (nrow(dailywts) != 0){

        for (p in 1:nrow(dailywts)){
          if (is.na(as.Date(dailywts$dateOUT[p]))){
            dates <- seq(as.Date(dailywts$dateIN[p]), Sys.Date(), by = "days")
            dates<-dates[between(dates, as.Date(start), as.Date(end))]
          }else{
            dates <- seq(as.Date(dailywts$dateIN[p]), as.Date(dailywts$dateOUT[p]), by = "days")
            dates<-dates[between(dates, as.Date(start), as.Date(end))]}

          if (length(dates) == 0){dailywts$name[p] <- ""}}

      dailywts <- dailywts %>% filter(name != "")}

    }else{

    n <- which(dailywts$name == paddock)

    if (length(n) != 0){

    for (p in 1:length(n)){
      if (is.na(as.Date(dailywts$dateOUT[n[p]]))){
        dates <- seq(as.Date(dailywts$dateIN[n[p]]), Sys.Date(), by = "days")
        dates<-dates[between(dates, as.Date(start), as.Date(end))]
      }else{
      dates <- seq(as.Date(dailywts$dateIN[n[p]]), as.Date(dailywts$dateOUT[n[p]]), by = "days")
      dates<-dates[between(dates, as.Date(start), as.Date(end))]}

      if (length(dates) == 0){dailywts$name[n[p]] <- ""}}}

      dailywts <- dailywts %>% filter(paddock %in% name)
      }

    #This is the section where we can apply further filters based on breed, class, etc.

    if (nrow(dailywts) != 0){
      cattleinfo[[jan2$properties$Management[i]]] <- as.data.frame(dailywts)}else{
        jan2$properties$Management[[i]] <- "xxxx"}
  }

  RFID <- jan2[which(jan2$properties$Management!="xxxx"),]
  cattleinfo <- list(Management=RFID$properties$Management, RFID=RFID$RFID, PaddockHistory=cattleinfo)

  return(cattleinfo)

}

