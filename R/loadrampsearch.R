#' Package with functions to enable easier code to access the DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' for a property from MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name loadrampsearch
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


loadrampsearch <- function(property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  loadramps <- mongo(collection = "LoadRamps", db = "DMIoT", url = pass, verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  filterstation <- sprintf('{"station":{"$in":["%s"]}, "actioned":"%s"}', property, "0")
  lookfor <- sprintf('{"RFID":true, "datetime":true, "station":true, "_id":false}')
  propertyinfo <- loadramps$find(query = filterstation, fields=lookfor)

  indb <- cattlesearch(propertyinfo$RFID, username=username, password=password)

  if (nrow(propertyinfo) == 0){propertyinfo1 <- propertyinfo}

  if (nrow(propertyinfo) >= 1 && class(indb) == "NULL"){
      propertyinfo1 <- propertyinfo%>%
        mutate(Management = "", category = "", breed = "", sex = "")%>%
    select("datetime","RFID","station","Management","category","breed","sex")%>%
        arrange(datetime)
    }

  if (nrow(propertyinfo) >= 1 && nrow(indb) >= 1){

  propertyinfo1 <- full_join(propertyinfo, indb, by = "RFID")%>%
    select("datetime","RFID","station","Management","category","breed","sex")%>%
    arrange(datetime)}

  return(propertyinfo1)
}
