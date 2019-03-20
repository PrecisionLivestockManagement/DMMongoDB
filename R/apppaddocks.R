#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of paddock polygons for a property from MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name apppaddocks
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import rgdal
#' @import geojsonio
#' @import cleangeo
#' @import sf
#' @export


apppaddocks <- function(property, username=NULL, password=NULL){

  filterstation <- sprintf('{"stationname":"%s"}', property)

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster",
    url = pass,
    verbose = T)

  temp <- mongo(collection = "Temp", db = "DataMuster",
    url = "mongodb://DavoSwain:8Ll7waS0z9ARtkX6@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin",
    verbose = T)

  temp$drop()

  property <- paste(unlist(property), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)

  tempadds <- paddocks$find(query = filterstation, fields='{"stationname":true, "geometry":true, "properties.hectares":true, "paddname":true, "_id":false}')
  temp$insert(tempadds)
  pads = tempfile(fileext=".json")
  temp$export(file(pads))
  pado <- sprintf('{"type" : "FeatureCollection", "features": [%s]}', paste(readLines(pads), collapse=","))
  write(pado, pads)
  PropPadds <- as(st_read(pads, quiet = TRUE), "Spatial")
  #PropPadds <- clgeo_Clean(geojson_read(pads, what = "sp"))
  temp$drop()
  PropPadds$id <- tempadds$paddname
  PropPadds@data$id <- tempadds$paddname
  ifelse(PropPadds$id=="xxxxxx",PropPadds$id <- row_number(PropPadds$id),PropPadds$id<-PropPadds$id)

  PropPadds$property <- tempadds$stationname

  return(PropPadds)


}
