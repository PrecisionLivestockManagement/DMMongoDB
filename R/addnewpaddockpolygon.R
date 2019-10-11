#' Add new user
#'
#' This function adds a new user to the MongoDB database. You can only access this function if you have read and write permission
#' @name addnewpaddockpolygon
#' @param property name of the property to be added
#' @param paddockname a shortened name of the property to be added
#' @param ha the size of the polygon in hectares
#' @param long longitude coordinate of a point on the property
#' @param lat latitude coordinate of a point on the property
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return message to say the user has been successfully added
#' @author Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addnewpaddockpolygon <- function(property, paddockname, ha, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    propertyinfo <- stationinfo(property)

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')

    template$stationname <- property
    template$stationID <- propertyinfo$`_id`
    template$paddname <- paddockname
    template$paddnum <- 1

    template$properties$hectares <- ha
    template$properties$sq_kms <- ha / 10000
    template$properties$acres <- ha * 2.41

    rownames(template)<-c()
    rownames(template$properties)<-c()
    rownames(template$geometry)<-c()

    paddocks$insert(template)


    long <- propertyinfo$longitude
    lat <- propertyinfo$latitude

    areasqm <- ha * 10000
    distfromcentre <- ((areasqm^0.5)/2)/100000

    c1lat <- lat + distfromcentre
    c1long <- long + distfromcentre
    c2lat <-  lat - distfromcentre
    c2long <- long + distfromcentre
    c3lat <- lat - distfromcentre
    c3long <- long - distfromcentre
    c4lat <- lat + distfromcentre
    c4long <- long - distfromcentre
    c5lat <-  c1lat
    c5long <- c1long

    coords <- paste0("[",c1long,",",c1lat,"],[",c2long,",",c2lat,"],[",c3long,",",c3lat,"],[",c4long,",",c4lat,"],[",c5long,",",c5lat,"]")

    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
    IDI <- sprintf('{"stationname":"%s"}', property)
    IDS <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coords)
    paddocks$update(IDI, IDS)

}
