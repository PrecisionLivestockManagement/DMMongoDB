#' Retrieve paddock shapefiles from the DataMuster MongoDB database.
#'
#' This function retrieves paddock shapefiles from the DataMuster MongoDB database.
#' @name get_paddock_shapefiles
#' @param property the name of the property to add the paddocks
#' @param filedir the location you wish to save the spatial files
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the shapefiles have been successfully saved
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import sf
#' @export

get_paddock_shapefiles <- function(property, filedir, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    #Retrieve paddock files from database

    propertyf <- sprintf('"stationname":"%s",', property)

    # Set up query to search for cattle

    filter <- paste0("{", propertyf, "}")
    filter <- substr(filter, 1 , nchar(filter)-2)
    filter <- paste0(filter, "}")

    lookfor <- sprintf('{"stationname":true, "geometry.coordinates":true, "properties.hectares":true, "paddname":true, "cattle":true, "_id":false}')

    padds <- paddocks$find(query = filter, fields = lookfor)

    padds <- padds %>%
      mutate(hectares = properties$hectares,
             geom = geometry$coordinates)%>%
      select("stationname", "paddname", "hectares", "cattle", "geom")

    paddinfo <- subset(padds, select = -geom)

    cattle = SpatialPolygons(lapply(1:nrow(padds), function(x) Polygons(list(Polygon(matrix(padds$geom[[x]], ncol = 2))), paste0("ID",x))),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

    PropPadds <- SpatialPolygonsDataFrame(cattle, paddinfo, match.ID=FALSE)

    for(i in 1:nrow(PropPadds@data)){

      PropPadds$long[i] <- PropPadds@polygons[[i]]@labpt[1]
      PropPadds$lat[i] <- PropPadds@polygons[[i]]@labpt[2]
    }

    padfiles <- PropPadds

    padnames <- get_paddocks(property = property, fields = c("paddname", "poly_paddname"), username = username, password = password)

    padfiles$paddname <- replace(padfiles$paddname, padfiles$paddname == padnames$paddname, padnames$poly_paddname)

    padfiles <- padfiles %>% rename("poly_paddname" = paddname)

    test <- st_as_sf(padfiles)

    #writeOGR(obj = padfiles, dsn = filedir, layer = "padfiles", driver = "ESRI Shapefile")

    st_write(obj = test, dsn = filedir, layer = "padfiles", driver = "ESRI Shapefile")

}
