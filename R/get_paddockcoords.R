#' Retrieve cattle paddock history from the DataMuster MongoDB database
#'
#' This function provides a list of cattle paddock history
#' for a property from DataMuster MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name get_paddockcoords
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param paddock this is the name of a paddock or list of paddocks as character entries, if no value is entered then all paddocks are loaded
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers and paddock history
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


get_paddockcoords <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )
  filterstation1 <- sprintf('{"stationname":{"$in":["%s"]}}', property)

  lookfor <- sprintf('{"stationname":true, "paddname":true, "geometry.coordinates":true, "properties.hectares":true, "_id":false}')

  tempadds <- paddocks$find(query = filterstation1, fields=lookfor)

  if(nrow(tempadds) != 0){tempadds <- tempadds%>%
    mutate(hectares = properties$hectares,
           geom = geometry$coordinates)%>%
    select("stationname", "paddname", "hectares", "geom")}else{

      tempadds <- data.frame()}

  cattle = SpatialPolygons(lapply(1:nrow(tempadds), function(x) Polygons(list(Polygon(matrix(tempadds$geom[[x]], ncol = 2))), paste0("ID",x))), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  cattleadd <- tempadds%>%select(stationname, paddname, hectares)

  PropPadds <- SpatialPolygonsDataFrame(cattle, cattleadd, match.ID=FALSE)

  return(PropPadds)

}


