#' Retrieve cattle information via the DataMuster website app
#'
#' This function provides a flexible search tool to find cattle based on a flexible number of search terms. It also allows the user to define what fields should be returned via a list of fields.
#' @name get_cattle
#' @param property property to search for
#' @param sex male or female
#' @param zoom whole property or paddock zoomchoice
#' @param paddock paddock on property
#' @param category the class of cattle either breeding or growing
#' @param alms TRUE or FALSE, if true filters the data for cattle currently allocated to an alms unit
#' @param timezone the timezone that applies to the cattle data
#' @param fields a list of MongoDB cattle collection headers that you want returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle with the list of fields defined in the inputs and searched using the search terms.
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_cattle <- function(RFID = NULL, property = NULL, sex = NULL, category = NULL, paddock = NULL, alms = NULL, weaned = NULL, id = NULL, almsasset_id = NULL,
                       exstation = NULL, exitdate = NULL, entrydate = NULL, deathdate = NULL, timezone = NULL, prevpaddock = NULL, active = NULL,
                       fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(timezone)){timezone <- "AUstralia/Brisbane"} else {}

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
    RFID <- paste(unlist(RFID), collapse = '", "' )
    RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)


# Set up find query

search <-paste0("{", property, sex, paddock, category, alms, weaned, id, almsasset_id, exstation, exitdate, entrydate, deathdate, RFID, prevpaddock, active,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- cattle$find(query = search, fields = snappy)

# This bit of code unlists dataframes within the dataframe

for(i in 1:ncol(data)){
  n <- length(data[,i])
  if(n > 1){
    data <- cbind(data, data[,i])
    data <- data[,-i]}
}

# #collist <- colnames(dataf)
#
# # if(nrow(data) !=0){
# # for(i in 1:length(collist)){
# #   if("POSIXt" %in% class(dataf[,i])){
# #     attributes(dataf[,i])$tzone <- timezone}}}

# s <- Sys.time()
# attr(s,"tzone") <- timezone

 if(nrow(data) != 0){
 dataf <- data%>%
              #  rename_all(recode, Management = "Tag", sex = "Sex", category = "Category", stwtdate = "Last Crush Weight Date",
              #                     stweight = "Weight (kg)", recordedtime = "Hours since last ALMS record", wkwtdate = "Last Average ALMS Weight Date", wkweight = "Weight (kg)")%>%
                #mutate_at(vars(ends_with("Date")), as.Date)#%>%
                #mutate_at(vars(ends_with("Date")), funs(as.Date(., tz = timezone)))
                mutate_at(vars(ends_with("Date")), as.Date, tz = timezone)
              #  mutate_at(vars(ends_with("Date")), funs(ifelse(. == "Jan 01 1970" | . == "Dec 31 1969", "", .)))%>%
              #  mutate_at(vars(starts_with("Weight")), funs(round(as.numeric(.), 0)))%>%
              #  mutate_at(vars(starts_with("Weight")), funs(ifelse(. == 0, as.character(""), as.character(.))))%>%
              #  mutate_at(vars(starts_with("Hours")), funs(round(as.numeric(difftime(s, ., units = "hours")),0)))%>%
              #  mutate_at(vars(starts_with("Hours")), funs(ifelse(. > 1000, NA, .)))%>%
              #  select(RFID, Tag, Sex, Category, Paddock, everything())%>%
              #  filter(RFID != "xxxxxx")
 }

if(!exists("dataf") | exists("dataf") && nrow(dataf) == 0){
  dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
           mutate_all(funs(as.character(.)))
  }

dataf

}

