#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of ALMS units with use history for a property from MongoDB. Inputs need to be a list of one or more property names
#' @name appinfs
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the data numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


appinfs <- function(property, start=NULL, end=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster",
    url = pass,
    verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}, "properties.type":"%s"}', property, "Walk-over-Weighing Unit")

  info <- list()

  jan2 <- infrastructure$find(query = filterstation, fields='{"properties.asset_id":true, "usehist.date":true, "usehist.num":true, "_id":false}')

  for(i in 1:length(jan2$properties$asset_id)){

    asset <- setNames(data.frame(matrix(ncol = 2, nrow = length(jan2$usehist$date[[i]]))), c("Date", "Num"))
    asset$Date <- jan2$usehist$date[[i]]
    asset$Num <- jan2$usehist$num[[i]]

    if(is.null(start)) {}
    else{if(is.null(end)){asset <- asset %>% filter(between(as.Date(Date),start,Sys.Date()))}
      else{asset <- asset %>% filter(between(as.Date(Date),start,end))}}

    info[[jan2$properties$asset_id[i]]] <- as.data.frame(asset)
    }

  data <- jan2
  info <- list(data=data$properties$asset_id, UseHist=info)

  return(info)

}

