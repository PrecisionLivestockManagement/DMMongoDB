#' Add new user
#'
#' This function adds a new user to the MongoDB database. You can only access this function if you have read and write permission
#' @name addnewstation
#' @param stationname name of the property to be added
#' @param stationshortname a shortened name of the property to be added
#' @param lat latitude coordinate of a point on the property
#' @param long longitude coordinate of a point on the property
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return message to say the user has been successfully added
#' @author Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addnewstation <- function(stationname, stationshortname, long, lat, PIC=NULL, timezone=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

    prop <- stations$find(query = '{}', fields = '{"_id":false}')

    if(stationname %in% prop$name) {stop("Warning: Matching station detected")}

    template <- prop[prop$name == "Tremere", ]

    #Input new station details into the template dataframe --------

    template$name<- stationname
    template$shortname <- stationshortname
    template$longitude<-long
    template$latitude<- lat
    template$PIC <- ifelse(!is.null(PIC), PIC, "xxxxxx")
    template$timezone <- ifelse(!is.null(timezone), timezone, "Australia/Brisbane")

    rownames(template)<-c()
    rownames(template$geometry)<-c()

    stations$insert(template)

}
