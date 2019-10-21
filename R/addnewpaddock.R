#' Add a new paddock
#'
#' This function adds a new paddock or paddocks to the DataMuster database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name addnewpaddock
#' @param property this is the name of the property
#' @param paddockname this is the name of the paddock or paddocks. If NULL, the paddock will be named as the paddock number
#' @param filedir the location of the spatial file
#' @param filename the name of the spatial file. If NULL, all spatial files in the directory will be read and added as paddocks
#' @param filetype the type of spatial file (e.g. shp, kmz)
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the paddock has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import sf
#' @import rgdal
#' @import leafletR
#' @export

addnewpaddock <- function(property, filedir, filetype, paddockname=NULL, filename=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    propertyinfo <- stationinfo(property)
    propertyid <- propertyinfo$`_id`

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')
    template$stationname <- property
    template$stationID <- propertyid
    template$properties$datasource <- filetype

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    setwd(filedir)

    #Read in file
    #Need to rename kmz as .kml.zip, unzip the folder then read the doc.kml file using st_read

    if(filetype == "kmz"){

      if(is.null(filename)){filename <- list.files(pattern = ".kmz")}

   for (i in 1:length(filename)){

     file <- filename[i]
     unzip(file)

    lyr <- ogrListLayers("doc.kml")

    foo = readOGR("doc.kml", lyr[1])

    pads = toGeoJSON(data=foo, name = "data", dest=tempdir())

    # When rgdal writes the geojson file it includes header information and commas at the end of lines - if we want to continue to have each document representing
    # a paddock we need to clean up the geojson file. I think this is the best way to go but maybe we need to discuss maintaining paddocks at the property level.

    jack <- readLines(pads)
    jack <- jack[-c(1:12)]
    jack <- jack[-length(jack)]
    jack <- jack[-length(jack)]
    jack <- jack[-length(jack)]
    jack <- jack[-length(jack)]

    jack1<-jack[1]

    jack2 <- substr(jack1, 27, nchar(jack1)-4)

    count <- paddocks$count(query = sprintf('{"stationname":"%s"}', property))

    template$paddname <- ifelse(is.null(paddockname),as.character(count+1), paddockname)

    template$paddnum <- as.integer(count+1)

    paddocks$insert(template)

    IDS <- sprintf('{"stationname":"%s", "paddnum":%s}', property, template$paddnum)
    IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', jack2)
    paddocks$update(IDS, IDI)
   }
      }

}
