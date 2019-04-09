#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' for a property from MongoDB. Inputs need to be a list of one or more property names
#' @name movecattle
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param paddock the name of the paddocks to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a spatialpointsdataframe with a list of the RFID numbers and a number of other data points, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import rgdal
#' @import splancs
#' @import spdplyr
#' @export


movecattle <- function(property, paddock=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  if (is.null(paddock)){
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property)}else{

  paddock <- paste(unlist(paddock), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}, "properties.Paddock":{"$in":["%s"]}}', property, paddock)}

  lookfor <- sprintf('{"stationname":true, "RFID":true, "properties.Management":true, "geometry":true, "properties.Paddock":true, "_id":true}')
  cattleinfo <- cattle$find(query = filterstation, fields=lookfor)


  cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(unlist(cattleinfo$geometry$coordinates), nrow=length(cattleinfo$geometry$coordinates), byrow=T)), cattleinfo$properties)

  cattleinfospatial@data["RFID"] <- cattleinfo$RFID
  cattleinfospatial@data["property"] <- cattleinfo$stationname
  cattleinfospatial@data["ID"] <- cattleinfo$`_id`

  pad <- apppaddocks(property, username = username, password = password)
  cat <- cattleinfospatial

  kipper <- lapply(1:nrow(cat), function(x) {
    snicker <- pad%>%filter(paddname==cat$Paddock[x])
    csr(snicker@polygons[[1]]@Polygons[[1]]@coords, 1)
  })

  for(i in 1:nrow(cat)){
    catman <- cat$ID[i]

  RFIDS <- sprintf('{"_id" : {"$oid":"%s"}}', catman)


  RFIDI <- sprintf('{"$set":{"geometry.coordinates.0":%s, "geometry.coordinates.1":%s}}', kipper[[i]][1],kipper[[i]][2])

  cattle$update(RFIDS, RFIDI)

  }

}
