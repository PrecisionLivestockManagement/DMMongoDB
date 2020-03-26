#' Retrieve infrastructure information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve infrastructure information from the Infrastructure collection in the DataMuster MongoDB database. It also allows the user to define what fields should be returned. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_infrastructure
#' @param property the name of the property to search for
#' @param type type of infrastructure
#' @param active TRUE or FALSE, if TRUE, filters the data for infrastructure that is currently active
#' @param training TRUE or FALSE
#' @param fields a list of headers from the Infrastructure collection in the DataMuster MongoDB database to be returned
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a list of cattle RFID numbers and associated infrastructure statistics
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import keyring
#' @export


get_infrastructure <- function(property = NULL, type = NULL, active = NULL, training = NULL, fields = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(property)){}else{property <- paste(unlist(property), collapse = '", "' )
                              property <- sprintf('"stationname":{"$in":["%s"]},', property)}

  if(is.null(type)){}else{type <- paste(unlist(type), collapse = '", "' )
  type <- sprintf('"properties.type":{"$in":["%s"]},', type)}

  if(is.null(active)){}else{active <- sprintf('"properties.datarecording":"%s",', active)}
  if(is.null(training)){}else{training <- sprintf('"properties.training":"%s",', training)}


pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

# Set up find query

search <-paste0("{", property, type, active, training,"}")

if(nchar(search)==2){}else{
search <- substr(search, 1 , nchar(search)-2)
search <- paste0(search, "}")}

# Set up find fields

snif <- sprintf('"%s":true', fields)
te <- paste0(snif, collapse = ", ")
snappy <- sprintf('{%s, "_id":false}', te)

#Query database and format for website display

data <- infrastructure$find(query = search, fields = snappy)

# If no data is returned an empty dataframe is created

if(nrow(data) == 0){
    dataf <- setNames(data.frame(matrix(ncol = length(fields), nrow = 0)), gsub(".*\\.","", fields))%>%
    mutate_all(funs(as.character(.)))}else{

# Brings all data up to the same level

for(i in 1:ncol(data)){
  class <- class(data[,i])
  if(class == "data.frame"){
    data <- cbind(data, data[,i])
    data <- data[,-i]}}

 dataf <- data%>%
          rename_all(recode, stationname = "Stationname", asset_id = "Asset_id", type = "Type")
}

dataf

}

