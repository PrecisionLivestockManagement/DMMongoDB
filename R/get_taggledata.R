#' Retrieve taggle data information from the DataMuster MongoDB database.
#'
#' This function provides a search tool to retrieve taggle data from the DMWater collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name get_taggledata
#' @param TagNo the Taggle tag number
#' @param Days number of days to go back and retrieve data for
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe that includes the Taggle tag ID plus the meter readings and the water use per hour
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export

get_taggledata <- function(TagNo, Days, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

TagNo <- TagNo

days <- Days

seconds <- days * 60 * 60 * 24

Time <- Sys.time() - seconds

trumpper <- strftime(Time, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT")

username <- username
password <- password

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
sensor <- mongo(collection = "DMWater", db = "DMIoT", url = pass, verbose = T)

snail <- sensor$find(query = sprintf('{"TagID": "%s", "DateTime": { "$gte" : { "$date" : "%s" }}}', TagNo, trumpper))
}


