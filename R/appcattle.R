#' Retrieves cattle from the DataMuster database
#'
#' This function provides a list of cattle for a property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name appcattle
#' @param property the name of the property to query
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a spatialpointsdataframe with a list of the RFID numbers and a number of other data points, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import rgdal
#' @export


appcattle <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)

  property1 <- paste(unlist(property), collapse = '", "' )
  filterstation <- sprintf('{"stationname":{"$in":["%s"]}}', property1)
  lookfor <- sprintf('{"stationname":true, "RFID":true, "properties.Management":true, "geometry":true, "properties.Paddock":true, "properties.sex":true, "properties.category":true, "properties.stweight":true, "properties.stwtdate":true, "properties.weight":true, "properties.recordedtime":true, "properties.wkweight":true, "properties.wkwtdate":true, "properties.ALMS":true, "_id":false}')
  cattleinfo <- cattle$find(query = filterstation, fields=lookfor)

  if (nrow(cattleinfo) != 0){

  cattleinfo$properties$RFID <- cattleinfo$RFID
  cattleinfo$properties$stationname <- cattleinfo$stationname
  cattleinfo$properties$geom <- cattleinfo$geometry$coordinates
  cattleinfo <- cattleinfo$properties}

  if(length(unique(cattleinfo$stationname)) != length(property)){

  missingprops <- property[!(property %in% cattleinfo$stationname)]

  cattleinfo1 <- cattle$find(query = sprintf('{"RFID":"xxxxxx"}'), fields=lookfor)
  cattleinfo1 <- cattleinfo1[rep(1:nrow(cattleinfo1),each=length(missingprops)),]

  cattleinfo1$stationname <- missingprops

  cattleinfo1$properties$RFID <- cattleinfo1$RFID
  cattleinfo1$properties$stationname <- cattleinfo1$stationname
  cattleinfo1$properties$geom <- cattleinfo1$geometry$coordinates
  cattleinfo1 <- cattleinfo1$properties

  cattleinfo <- rbind(cattleinfo, cattleinfo1)
  }

  cattleinfo$stwtdate <- as.Date(cattleinfo$stwtdate, tz = "Australia/Brisbane")
  cattleinfo$wkwtdate <- as.Date(cattleinfo$wkwtdate, tz = "Australia/Brisbane")

  cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(unlist(cattleinfo$geom), nrow=length(cattleinfo$geom), byrow=T)), cattleinfo%>%select(-"geom"))

  cattleinfospatial <- cattleinfospatial %>% rename(property = stationname)

  #cattleinfospatial <- SpatialPointsDataFrame(data.frame(matrix(c(0, 0), nrow=1, ncol=2, byrow = TRUE)), cattleinfo)

  return(cattleinfospatial)

}
