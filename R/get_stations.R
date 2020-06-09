#' Retrieve station information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve station information from the Stations collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_stations
#' @param stationname the name of the property to search for
#' @param report users to search for
#' @param fields a list of headers from the Stations collection in the DataMuster MongoDB database to be returned. If not specified, the property name, hectares, timezone, longitude, and latitude will be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated station information
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_stations <- function(stationname = NULL, report = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(stationname)){} else {
    stationname <- paste(unlist(stationname), collapse = '", "' )
    stationname <- sprintf('"stationname":{"$in":["%s"]},', stationname)}

  if(is.null(report)){}else{report <- sprintf('"reports.name":"%s",', report)}

  if(is.null(fields)){
    fields = c("stationname", "hectares", "timezone", "longitude", "latitude")}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", stationname, report,"}")

  if(nchar(search)==2){}else{
    search <- substr(search, 1 , nchar(search)-2)
    search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":true}', te)

#Query database and format for website display

data <- stations$find(query = search, fields = snappy)

# This bit of code unlists dataframes within the dataframe

for(i in 1:ncol(data)){
  class <- class(data[,i])
  if(class == "data.frame"){
    data <- cbind(data, data[,i])
    data <- data[,-i]}
}


#collist <- colnames(stationdataf)

# for(i in 1:length(collist)){
#   if("POSIXt" %in% class(cattledataf[,i])){
#     attributes(cattledataf[,i])$tzone <- timezone}}

# s <- Sys.time()
# attr(s,"tzone") <- timezone

if(nrow(data) != 0){
 dataf <- data%>%
                 rename_all(recode, stationname = "Stationname", name = "Report name", email = "Report email")
#                mutate_at(vars(ends_with("Date")), as.character, format = "%b %d %Y")%>%
#                mutate_at(vars(ends_with("Date")), funs(ifelse(. == "Jan 01 1970" | . == "Dec 31 1969", "", .)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(round(as.numeric(.), 0)))%>%
#                mutate_at(vars(starts_with("Weight")), funs(ifelse(. == 0, as.character(""), as.character(.))))%>%
#                mutate_at(vars(starts_with("Hours")), funs(round(as.numeric(difftime(s, ., units = "hours")),0)))%>%
#                mutate_at(vars(starts_with("Hours")), funs(ifelse(. > 1000, NA, .)))%>%
#                select(RFID, Tag, Sex, Category, Paddock, everything())%>%
#                filter(RFID != "xxxxxx")
}


# if(!exists("cattledataf") | exists("cattledataf") && nrow(cattledataf) == 0){
# cattledataf <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("RFID", "Tag", "Sex", "Category", "Paddock"))}

dataf

}

