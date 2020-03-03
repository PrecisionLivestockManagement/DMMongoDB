#' Add a station to the DataMuster database
#'
#' This function adds a station to the DataMuster database. You can only access this function if you have read and write permission
#' @name addnewstation
#' @param stationname the name of the property
#' @param lat the latitude of a coordinate point to locate the property
#' @param long the longitude of a coordinate point to locate the property
#' @param area the area of the staion in hectares, default is 100 ha
#' @param timezone the local timezone of the property, see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for the list of accepted timezones
#' @param PIC the Property Identification Code
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return message to say the station has been successfully added
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addnewstation <- function(stationname, long, lat, area=NULL, PIC=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    prop <- stations$find(query = '{}', fields = '{"_id":false}')

    if(stationname %in% prop$stationname) {stop("Warning: Matching station detected")}

    template <- prop[prop$stationname == "Tremere", ]

    if(is.null(area)){area <- 100}
    if(is.null(PIC)){PIC <- "xxxxxx"}

    #Input new station details into a template dataframe and insert into database --------

    template$stationname<- stationname
    template$longitude<-long
    template$latitude<- lat
    template$PIC <- PIC
    template$hectares <- area
    template$timezone <- timezone

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    stations$insert(template)

    #Generate the polygon coordinates and update the Stations collection

    areasqm <- area * 10000
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

    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    IDI <- sprintf('{"stationname":"%s"}', stationname)
    IDS <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coords)
    stations$update(IDI, IDS)

    #Create a temporary paddock in the Paddocks collection using the polygon coordinates. This will be removed if other paddocks are added.

    propertyinfo <- stationinfo(stationname, username = username, password = password)
    propertyid <- propertyinfo$`_id`

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')
    template$stationname <- stationname
    template$stationID <- propertyid
    template$properties$datasource <- "stationpolygon"
    template$properties$hectares <- area
    template$paddname <- stationname
    template$paddnum <- 1

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    paddocks$insert(template)

    IDS <- sprintf('{"stationname":"%s"}', stationname)
    IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coords)
    paddocks$update(IDS, IDI)

}
