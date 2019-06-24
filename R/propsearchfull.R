#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' for a property from MongoDB including life data. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name propsearchfull
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param paddock this is the name of a paddock or list of paddocks as character entries, if no value is entered then all paddocks are loaded
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the RFID numbers, associated management tags and current paddocks the cattle are in
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


propsearchfull <- function(property, paddock=NULL, archives=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster",
    url = pass,
    verbose = T)

  property <- paste(unlist(property), collapse = '", "' )

  filterstation1 <- sprintf('{"stationname":{"$in":["%s"]}}', property)
  filterstation2 <- sprintf('{"$or": [{"exstation":"%s"}, {"stationname":"%s"}]}', property, property)

  lookfor <- sprintf('{"RFID":true, "properties.Management":true, "properties.Paddock":true, "properties.sex":true,
                     "properties.birthDate":true, "properties.damRFID":true, "properties.sireRFID":true,
                     "properties.breed":true, "properties.colour":true, "properties.brand":true, "properties.exitDate":true, "properties.deathDate":true,
                     "properties.horn":true, "properties.category":true, "properties.weaned":true, "properties.ALMS":true, "properties.ALMSasset_id":true, "properties.wkwtdate":true, "properties.birthWeight":true, "_id":false}')


  if(is.null(archives) || archives == "FALSE"){
    propertyinfo <- cattle$find(query = filterstation1, fields=lookfor)}else{
      propertyinfo <- cattle$find(query = filterstation2, fields=lookfor)}

  propertyinfo$properties["RFID"] <- propertyinfo$RFID

  propertyinfo <- propertyinfo$properties

  propertyinfo$birthDate <- as.Date(propertyinfo$birthDate, tz = "Australia/Brisbane")
  propertyinfo$wkwtdate <- as.Date(propertyinfo$wkwtdate, tz = "Australia/Brisbane")
  propertyinfo$exitDate <- as.Date(propertyinfo$exitDate, tz = "Australia/Brisbane")
  propertyinfo$deathDate <- as.Date(propertyinfo$deathDate, tz = "Australia/Brisbane")

  if(is.null(paddock)){}else{
    propertyinfo <- propertyinfo %>% filter(Paddock %in% paddock)}

  return(propertyinfo)

}
