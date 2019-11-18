#' Retrieves paddocks from the DataMuster database
#'
#' This function provides a list of paddock polygons for a property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name apppaddocks
#' @param property the name of the property to query
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a spatialpolygonsdataframe with a list of the paddock names and associated polygon coordinates
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import sp
#' @export


apppaddocks <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

  property1 <- paste(unlist(property), collapse = '", "' )

  filterpaddocks <- sprintf('{"stationname":{"$in":["%s"]}}', property1)

  tempadds <- paddocks$find(query = filterpaddocks, fields='{"stationname":true, "geometry":true, "properties.hectares":true, "paddname":true, "_id":false}')

  if(nrow(tempadds) != 0){tempadds <- tempadds%>%
                                      mutate(hectares = properties$hectares,
                                             geom = geometry$coordinates)%>%
                                      select("stationname", "paddname", "hectares", "geom")}else{

                                        tempadds <- data.frame()}

  missingprops <- property[!(property %in% tempadds$stationname)]

  if(length(missingprops)!=0){

    property2 <- paste(unlist(missingprops), collapse = '", "' )

    filterstations <- sprintf('{"name":{"$in":["%s"]}}', property2)

    tempadds1 <- stations$find(query = filterstations, fields='{"name":true, "geometry":true, "hectares":true, "_id":false}')

    tempadds1 <- tempadds1%>%
                 mutate(paddname = "1",
                        geom = geometry$coordinates)%>%
                 rename(stationname = name)%>%
                  select("stationname", "paddname", "hectares", "geom")

    tempadds <- rbind(tempadds, tempadds1)
  }

  cattle = SpatialPolygons(lapply(1:nrow(tempadds), function(x) Polygons(list(Polygon(matrix(tempadds$geom[[x]], ncol = 2))), paste0("ID",x))), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  cattleadd <- tempadds%>%select(stationname, paddname, hectares)


  PropPadds <- SpatialPolygonsDataFrame(cattle, cattleadd, match.ID=FALSE)

  return(PropPadds)

}
