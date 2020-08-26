#' Retrieve calving data from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve calving data from the CalvingData collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_DoBData
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle MTag/s. Property must be specified in order to search using this field
#' @param property the name of the property to search for
#' @param multiples whether the cow had multiples or not (TRUE/FALSE)
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param season the calving season, e.g. "2019/20"
#' @param cow_id a list of DataMuster database cow identification numbers
#' @param fields a list of headers from the CalvingData collection in the DataMuster MongoDB database to be returned. If not specified, the RFID, MTag, property, date of foetal aging, age at foetal aging, calving date, and twin status will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated static weight records
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_DoBData <- function(RFID = NULL, MTag = NULL, property = NULL, multiples = NULL, season = NULL, cow_id = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(RFID)){}else{RFID <- paste(unlist(RFID), collapse = '", "' )
  RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  if(is.null(MTag)){}else{MTag <- paste(unlist(MTag), collapse = '", "' )
  MTag <- sprintf('"Management":{"$in":["%s"]},', MTag)}

  if(is.null(property)){}else{property <- paste(unlist(property), collapse = '", "' )
  property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  if(is.null(multiples)){}else{multiples <- paste(unlist(multiples), collapse = '", "' )
  multiples <- sprintf('"multiples":{"$in":["%s"]},', multiples)}

  if(is.null(season)){}else{
    season <- sprintf('"season":{"$in":["%s"]},', season)}

  if(is.null(cow_id)){}else{cow_id <- paste(unlist(cow_id), collapse = '", "' )
  cow_id <- sprintf('"_id":{"$in":["%s"]},', cow_id)}

  if(is.null(fields)){
    fields = c("RFID", "Management", "stationname", "calvingdate", "multiples", "season")}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

dobdata <- mongo(collection = "DoBData", db = "DMAlgorithms", url = pass, verbose = T)

# Set up find query

search <-paste0("{", RFID, MTag, property, multiples, season, cow_id,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- dobdata$find(query = search, fields = snappy)

# If no data is returned an empty dataframe is created

if(nrow(data) == 0){
  dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}else{

collist <- colnames(data)

timezone <- "Australia/Brisbane"

 for(i in 1:length(collist)){
   if("POSIXt" %in% class(data[,i])){
     attributes(data[,i])$tzone <- timezone
     data[,i] <- as.Date(data[,i], tz = timezone)
     }}

# dataf <- data%>%
#          mutate_if(~'POSIXt' %in% class(.x), format(as.Date(.x, tz = timezone)))

 dataf <- data

    }

dataf

}

