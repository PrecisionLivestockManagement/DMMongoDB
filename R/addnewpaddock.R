#' Add a new paddock
#'
#' This function adds a new paddock or paddocks to the DataMuster database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name addnewpaddock
#' @param property this is the name of the property
#' @param paddockname this is the name of the paddock or paddocks
#' @param filedir the location of the spatial file
#' @param filename the name of the spatial file
#' @param filetype the type of spatial file (e.g. shp, kmz)
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the paddock has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import sf
#' @import rgdal
#' @export

addnewpaddock <- function(property, paddockname, filedir, filename, filetype, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    propertyinfo <- stationinfo(property)
    propertyid <- propertyinfo$`_id`

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')
    count <- paddocks$count(query = sprintf('{"stationname":"%s"}', property))

    template$stationname <- property
    template$stationID <- propertyid
    template$paddname <- paddockname
    template$properties$datasource <- filetype
    template$paddnum <- count+1

    #Read in file
    #Need to rename kmz as .kml.zip, unzip the folder then read the doc.kml file using st_read

    if(filetype == "kmz"){

    setwd(filedir)

    unzip(filename)

    #file <- st_read("doc.kml")


    foo = readOGR("doc.kml", lyr[1])

    pads = toGeoJSON(data=foo, name = "data", dest=tempdir())

    #file <- file[3]

    #file$geometry[[1]][[1]] <- matrix(file$geometry[[1]][[1]][file$geometry[[1]][[1]] != 0], ncol = 2)

    #file[2:7] <- template$properties

    #PropPadds <- st_transform(file, CRS("+proj=longlat +datum=WGS84 +no_defs"))

    # Output spatial dataframe as a geojson file with the MongoDB format, rather than worrying about directories etc just create a temporary file
    #pads = tempfile(fileext=".geojson")

    # Write the spatial dataframe to the temporary geojson file
    #st_write(PropPadds, pads, "GeoJSON", driver="GeoJSON")
    }

    # When rgdal writes the geojson file it includes header information and commas at the end of lines - if we want to continue to have each document representing
    # a paddock we need to clean up the geojson file. I think this is the best way to go but maybe we need to discuss maintaining paddocks at the property level.

    jack <- readLines(pads)
    jack <- jack[-c(1:5)]
    jack <- jack[-length(jack)]
    jack <- jack[-length(jack)]

    for (i in 1:length(jack)) {

      n <- count+i

      jack[i] <- gsub ('^(.{20})(.*)$', paste0("\\1 \"stationname\": \"",property,"\", \"stationID\": \"" , propertyid,"\",  \"paddname\": \"",paddockname,"\",\"paddnum\": \"", n,"\",\\2"),jack[i])

      jack[i] <- gsub(gsub (".*paddnum\":\\s*|,.*","",jack[i]), n, jack[i])

      if (i != length(jack)) {jack[i] <- gsub ( '.{1}$','',jack[i])}

    }

    paddocks$insert(jack)

}
