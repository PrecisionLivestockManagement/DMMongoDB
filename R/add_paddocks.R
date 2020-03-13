#' Add paddocks to the DataMuster MongoDB database
#'
#' This function adds individual or groups of paddocks to the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_paddocks
#' @param property the name of the property to add the paddocks
#' @param paddockname the name of the paddock/s, if NULL the paddock will be assigned a numeric name, if "filename" the name of the file will be assigned
#' @param filedir the location of the spatial file containing the paddock coordinates
#' @param filename the name of the spatial file, if NULL all spatial files in the directory will be read and added as paddocks
#' @param filetype the type of spatial file (e.g. shp, kmz, coords), if inputting raw coordinates they must be listed in pairs with long first then lat, if in decimal degrees minutes they must be in the format "35d9'58.12\"S"  "66d40'42.74\"W"
#' @param coords the coordinates of the paddock, if inputting raw coordinates they must be listed in pairs with long first then lat, if in decimal degrees minutes they must be in the format "35d9'58.12\"S"  "66d40'42.74\"W"
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the paddock/s have been successfully added
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import rgdal
#' @import leafletR
#' @export

add_paddocks <- function(property, paddockname=NULL, filedir=NULL,filename=NULL, filetype, coords=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)

    propertyinfo <- get_stations(property, fields = c("_id", "longitude", "latitude", "reports", "PIC", "timezone", "stationname"))
    propertyid <- propertyinfo$`_id`

    #Remove existing stationpolygon if exists --------

    filterpaddocks <- sprintf('{"stationname":"%s", "properties.datasource":"%s"}', property, "stationpolygon")

    padmatch <- paddocks$find(query = filterpaddocks, fields = '{"_id":true}')

    if(nrow(padmatch != 0)){
      IDS <- sprintf('{"_id":{"$oid":"%s"}}', padmatch$`_id`)
      paddocks$remove(IDS)
    }

    #Create a template dataframe to insert into database --------

    template <- paddocks$find(query = '{"stationname":"xxxxxx"}', fields = '{"_id":false}')
    template$stationname <- property
    template$stationID <- propertyid
    template$properties$datasource <- filetype

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    #Read in spatial file/s

    #KMZ files; need to unzip the folder then read the doc.kml file

    if(filetype == "kmz"){

      if(is.null(filename)){filename <- list.files(path = filedir, pattern = ".kmz")}

      if(!is.null(paddockname)){

        if(paddockname == "filename"){paddockname <- sub("\\..*","",filename)}}

      for (i in 1:length(filename)){

        file <- filename[i]
        filepath <- paste0(filedir, "/", file)
        temppath <- file.path(tempdir(), "doc.kml")

        unzip(zipfile = filepath, exdir=tempdir())
        lyr <- ogrListLayers(temppath)
        foo = suppressWarnings(readOGR(temppath, lyr[1]))
        pads = toGeoJSON(data=foo, name = "data", dest=tempdir())

        #getClass("Polygon")
        area<-sapply(slot(foo, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
        area <- area*1000000 #Convert to ha

        jack <- readLines(pads)
        jack <- jack[13]
        jack <- substr(jack, 27, nchar(jack)-4)

        #Add paddname and paddnum to the template dataframe and insert into the database
        count <- paddocks$count(query = sprintf('{"stationname":"%s"}', property))
        template$paddname <- ifelse(is.null(paddockname), as.character(count+1), paddockname[i])
        template$paddnum <- as.integer(count+1)
        template$properties$hectares <- area
        paddocks$insert(template)

        #Update paddock with the coordinates
        IDS <- sprintf('{"stationname":"%s", "paddnum":%s}', property, template$paddnum)
        IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', jack)
        paddocks$update(IDS, IDI)
   }
    }

    #KML files

    if(filetype == "kml"){

      if(is.null(filename)){filename <- list.files(path = filedir, pattern = ".kml")}

      if(!is.null(paddockname)){

      if(paddockname == "filename"){paddockname <- sub("\\..*","",filename)}}

      for (i in 1:length(filename)){

        file <- filename[i]
        filepath <- paste0(filedir, "/", file)
        #temppath <- file.path(tempdir(), "doc.kml")

        #unzip(zipfile = filepath, exdir=tempdir())
        lyr <- ogrListLayers(filepath)
        foo = suppressWarnings(readOGR(filepath, lyr[1]))
        pads = toGeoJSON(data=foo, name = "data", dest=tempdir())

        #getClass("Polygon")
        area<-sapply(slot(foo, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
        area <- area*1000000 #Convert to ha

        jack <- readLines(pads)
        jack <- jack[13]
        jack <- substr(jack, 27, nchar(jack)-4)

        #Add paddname and paddnum to the template dataframe and insert into the database
        count <- paddocks$count(query = sprintf('{"stationname":"%s"}', property))
        template$paddname <- ifelse(is.null(paddockname), as.character(count+1), paddockname[i])
        template$paddnum <- as.integer(count+1)
        template$properties$hectares <- area
        paddocks$insert(template)

        #Update paddock with the coordinates
        IDS <- sprintf('{"stationname":"%s", "paddnum":%s}', property, template$paddnum)
        IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', jack)
        paddocks$update(IDS, IDI)
      }
    }


    #Coordinates

    if(filetype == "coords"){

      coordinatelist <- ""

      for(i in 1:length(coords)){

        coord <- coords[[i]]

        if(substr(coord[1],3,3) == "d"){ #converts coordinates to decimal degrees
        coord <- char2dms(coord)
        coord <- as.numeric(coord)
      }

        coord <- paste0("[",round(coord[1],6),",",round(coord[2],6),"]")

        coordinatelist <- ifelse(i == length(coords), paste0(coordinatelist, coord), paste0(coordinatelist, coord, sep = ","))
      }

      #Add paddname and paddnum to the template dataframe and insert into the database
      count <- paddocks$count(query = sprintf('{"stationname":"%s"}', property))
      template$paddname <- ifelse(is.null(paddockname), as.character(count+1), paddockname)
      template$paddnum <- as.integer(count+1)

      rownames(template)<-c()
      rownames(template$geometry)<-c()

      paddocks$insert(template)

      #template$properties$hectares <- area

      IDS <- sprintf('{"stationname":"%s"}', stationname)
      IDI <- sprintf('{"$set":{"geometry.coordinates":[[%s]]}}', coordinatelist)
      paddocks$update(IDS, IDI)
      }

}
