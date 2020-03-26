#' Retrieve paddock information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve paddock information from the Paddocks collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_paddocks
#' @param property the name of the property to search for
#' @param fields a list of headers from the Paddocks collection in the DataMuster MongoDB database to be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated paddock information
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_paddocks <- function(property = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  # if(is.null(timezone)){timezone <- "Australia/Brisbane"} else {}

  if(is.null(property)){} else {
    property <- paste(unlist(property), collapse = '", "' )
    property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  cattle <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)


  # Set up find query

  search <-paste0("{", property,"}")

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
    class <- class(data[,i])
    if(class == "data.frame"){
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
#
#   if(nrow(dataf) != 0){
#     dataf <- dataf%>%
#       #  rename_all(recode, Management = "Tag", sex = "Sex", category = "Category", stwtdate = "Last Crush Weight Date",
#       #                     stweight = "Weight (kg)", recordedtime = "Hours since last ALMS record", wkwtdate = "Last Average ALMS Weight Date", wkweight = "Weight (kg)")%>%
#       #mutate_at(vars(ends_with("Date")), as.Date)#%>%
#       #mutate_at(vars(ends_with("Date")), funs(as.Date(., tz = timezone)))
#       mutate_at(vars(ends_with("Date")), as.Date, tz = timezone)
#     #  mutate_at(vars(ends_with("Date")), funs(ifelse(. == "Jan 01 1970" | . == "Dec 31 1969", "", .)))%>%
#     #  mutate_at(vars(starts_with("Weight")), funs(round(as.numeric(.), 0)))%>%
#     #  mutate_at(vars(starts_with("Weight")), funs(ifelse(. == 0, as.character(""), as.character(.))))%>%
#     #  mutate_at(vars(starts_with("Hours")), funs(round(as.numeric(difftime(s, ., units = "hours")),0)))%>%
#     #  mutate_at(vars(starts_with("Hours")), funs(ifelse(. > 1000, NA, .)))%>%
#     #  select(RFID, Tag, Sex, Category, Paddock, everything())%>%
#     #  filter(RFID != "xxxxxx")
#   }

  if(!exists("data") | exists("data") && nrow(data) == 0){
    data <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
      mutate_all(funs(as.character(.)))
  }

  data

}

