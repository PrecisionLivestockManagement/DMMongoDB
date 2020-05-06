#' Retrieve cattle information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve cattle information from the Cattle collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_cattle
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tag number/s
#' @param property the name of the property to search for
#' @param sex male or female
#' @param category the class of cattle either (breeding or growing)
#' @param paddock the name of the paddock to search for
#' @param alms TRUE or FALSE, if true filters the data for cattle currently allocated to an alms unit
#' @param weaned TRUE if the animal/s are weaned or FALSE if animal/s are not weaned
#' @param id ID of the animal
#' @param almsasset_id ID of the ALMS unit
#' @param exstation the former property of the animal
#' @param exitdate the date that the animal left the property in date format
#' @param entrydate the date that the animal entered the property in date format
#' @param deathdate the date that the animal died
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones
#' @param prevpaddock the former paddock of the animal
#' @param active TRUE or FALSE, if true filters the data for cattle that are currently active
#' @param fields a list of headers from the Cattle collection in the DataMuster MongoDB database to be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers with the list of fields defined in the inputs and searched using the search terms
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_cattle <- function(RFID = NULL, MTag = NULL, property = NULL, sex = NULL, category = NULL, paddock = NULL, alms = NULL, weaned = NULL, id = NULL, almsasset_id = NULL, exstation = NULL, exitdate = NULL, entrydate = NULL, deathdate = NULL, timezone = NULL, prevpaddock = NULL, active = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(property) & !is.null(MTag)){
    stop(paste0("To search using the management tag, please ensure the property field is filled out"))}


  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(property)){} else {
  property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  if(is.null(sex)){} else {sex <- sprintf('"properties.sex":"%s",', sex)}
  if(is.null(category)){} else {category <- sprintf('"properties.category":"%s",', category)}
  if(is.null(paddock)){}else{paddock <- sprintf('"properties.Paddock":"%s",', paddock)}
  if(is.null(alms)){} else {alms <- sprintf('"properties.ALMS":"%s",', alms)}
  if(is.null(weaned)){} else {weaned <- sprintf('"properties.weaned":"%s",', weaned)}
  if(is.null(exstation)){} else {exstation <- sprintf('"exstation":"%s",', exstation)}
  if(is.null(prevpaddock)){} else {prevpaddock <- sprintf('"properties.PrevPaddock":"%s",', prevpaddock)}
  if(is.null(active)){} else {active <- sprintf('"active":"%s",', active)}
  if(is.null(id)){} else {id <- sprintf('"_id":{"$oid":"%s"},', id)}

  if(is.null(exitdate)){} else {exitdate <- sprintf('"properties.exitDate":{"$gte":{"$date":"%s"}},', strftime(paste0(exitdate, "00:00:00"), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}
  if(is.null(entrydate)){} else {entrydate <- sprintf('"properties.entryDate":{"$gte":{"$date":"%s"}},', strftime(paste0(entrydate, "00:00:00"), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}
  if(is.null(deathdate)){} else {deathdate <- sprintf('"properties.deathDate":{"$gte":{"$date":"%s"}},', strftime(paste0(deathdate, "00:00:00"), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))}

  if(is.null(almsasset_id)){} else {
    almsasset_id <- paste(unlist(almsasset_id), collapse = '", "' )
    almsasset_id <- sprintf('"properties.ALMSasset_id":{"$in":["%s"]},', almsasset_id)}

  if(is.null(RFID)){} else {
    rfid <- RFID
    RFID <- paste(unlist(RFID), collapse = '", "' )
    RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(MTag)){} else {
    mtag <- MTag
    MTag <- paste(unlist(MTag), collapse = '", "' )
    MTag <- sprintf('"properties.Management":{"$in":["%s"]}}', MTag)}


pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


# Set up find query

search <- paste0("{", property, sex, paddock, category, alms, weaned, id, almsasset_id, exstation, exitdate, entrydate, deathdate, RFID, prevpaddock, active, MTag, "}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":true}', te)

#Query database and format for website display

data <- cattle$find(query = search, fields = snappy)

# If no data is returned an empty dataframe is created

if(nrow(data) == 0){
  data <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}

      # Brings all data up to the same level

      for(i in 1:ncol(data)){
        class <- class(data[,i])
        if(length(class) == 1 && class == "data.frame"){
          data <- cbind(data, data[,i])
          data <- data[,-i]}}

# If RFID was used as a search term, this section searches the database for RFID numbers that were not found in the database
# It looks at the RFID history to try to find the missing RFIDs
# If an RFID is found it will be added to the returned results with a message notifying the replacement RFID number

if(!is.null(RFID)){

missing <- rfid[!(rfid %in% data$RFID)]

if (length(missing) != 0){

  missingrfids <- paste(unlist(missing), collapse = '", "' )

  for(i in 1:length(missing)){
    search1 <- sprintf('{"RFIDhist.ID":{"$in":["%s"]}}', missingrfids[i])
    data1 <- cattle$find(query = search1, fields = snappy)

    if(nrow(data1) == 0){
      print(paste0("RFID ", missing[i], " cannot be found"))}else{
        print(paste0("RFID ", missing[i], " has been replaced with ", data1$RFID))

        for(i in 1:ncol(data1)){
          class <- class(data1[,i])
          if(length(class) == 1 && class == "data.frame"){
            data1 <- cbind(data1, data1[,i])
            data1 <- data1[,-i]}}

              data <- rbind(data, data1)}}}
}

# Formats any date columns with the correct timezone

 collist <- colnames(data)

  for(i in 1:length(collist)){
    if("POSIXt" %in% class(data[,i])){
      attributes(data[,i])$tzone <- timezone}}


 dataf <- data%>%
          mutate_at(vars(ends_with("Date")), as.Date, tz = timezone)


return(dataf)

}
