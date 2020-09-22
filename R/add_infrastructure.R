#' Add infrastructure to the DataMuster MongoDB database.
#'
#' This function adds infrastructure to the DataMuster MongoDB database. You can only access this function if you have read and write permissions.
#' @name add_infrastructure
#' @param property the name of the property to add the infrastructure
#' @param paddock the name of the paddock to add the infrastructure
#' @param infstype the infrastructure type (e.g. Walk-over-Weighing Unit, EID Reader, Water sensor)
#' @param assetID the asset identification number
#' @param fileout TRUE if the data is written to file or FALSE if the data is sent direct to the DataMuster MongoDB database, default is FALSE
#' @param active TRUE if the infrastructure is actively recording data or FALSE if the infrastructure is not yet recording data, default is TRUE
#' @param training only applicable for Walk-over-Weighing Units, TRUE if the herd is currently under training or FALSE if the herd is already trained, default is FALSE
#' @param telemetry only applicable for Walk-over-Weighing Units, TRUE if the Unit emits an hourly telemetry signal or FALSE if the Unit does not emit an hourly telemetry signal, default is FALSE
#' @param date the date that the infrastructure was installed in date format, default is today's date
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password, contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the infrastructure has been successfully added
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


add_infrastructure <- function(property, paddock, infstype, assetID, filename, active=NULL,
                               training=NULL, telemetry=NULL, date=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
    infrastructure <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

    propertyinfo <- get_stations(property, fields = c("_id", "longitude", "latitude", "reports", "PIC", "timezone", "stationname"))

    template <- infrastructure$find(query = '{"stationname": "xxxxxx"}', fields='{"_id":false}')

    if(is.null(date)){date <- Sys.Date()}else{}

    #Input new infrastructure details into the template dataframe --------

    template$stationname <- property
    template$stationID <- propertyinfo$`_id`

    template$properties$asset_id <- assetID
    template$properties$type <- infstype
    template$properties$filename <- filename
    template$properties$datarecording <- ifelse(is.null(active), "TRUE", active)
    template$properties$training <- ifelse(is.null(training), "FALSE", training)
    template$properties$telemetry_out <- ifelse(is.null(telemetry), "FALSE", telemetry)
    template$properties$Paddock <- paddock

    template$paddock <- list(paddock)

    template$pdkhist$name <- list(paddock)
    template$pdkhist$ID <- list("xxxxxx")
    template$pdkhist$dateIN <- list(as.POSIXct(date))

    rownames(template)<-c()
    rownames(template$geometry)<-c()
    rownames(template$properties)<-c()

    infrastructure$insert(template)

}
