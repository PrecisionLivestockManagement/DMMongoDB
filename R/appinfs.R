#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of ALMS units with use history for a property from MongoDB. Inputs need to be a list of one or more property names
#' @name appinfs
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import xts
#' @export


appinfs <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster",
    url = pass,
    verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)

  assetlist <- list()

  tempinfra <- infrastructure$find(query = filterstation, fields='{"properties.asset_id":true, "usehist.date":true, "usehist.num":true, "_id":false}')
  assetlistname <- list()
  for(i in 1:nrow(tempinfra)){
    if(length(tempinfra$usehist$date[[i]][])==0){}else{
      asset <- as.data.frame(tempinfra$usehist$date[[i]][])
      asset["Num"] <- as.data.frame(tempinfra$usehist$num[[i]][])
      asset1 <<- asset
      colnames(asset) <- c("Date","Num")
      asset <- xts(asset$Num,asset$Date)
      assign(as.character(as.character(tempinfra$properties$asset_id[i])), asset, envir = .GlobalEnv)
      assetlistname <- rbind(as.character(tempinfra$properties$asset_id[i]),assetlistname)
    }}

  if(nrow(tempinfra)==0){}else{
    values$assetlistname <- assetlistname
    values$asset1 <- asset1}



  return(values)

}


