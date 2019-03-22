#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function provides a list of cattle
#' usage of an ALMS unit for a property from MongoDB. Inputs need to be a list of one or more property names and if only one property a paddock name can be included
#' @name ALMSuse
#' @param property the name of the property to search the DataMuster MongoDB Atlas server
#' @param paddock this is the name of a paddock or list of paddocks as character entries, if no value is entered then all paddocks are loaded
#' @param start provide a start date to be returned, this has to be in date format.
#' @param end provide a end date to be returned, this has to be in date format.
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe with a list of the cattle numbers associated with the ALMS and the number of cattle recorded
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


ALMSuse <- function(property, paddock=NULL, start=NULL, end=NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  inf <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)

  type <- "Walk-over-Weighing Unit"

  property <- paste(unlist(property), collapse = '", "' )
  paddock <- paste(unlist(paddock), collapse = '", "' )

  if(is.null(paddock)) {
    filterstation <- sprintf('{"stationname":"%s", "properties.type":"%s"}', property, type)} else {
    filterstation <- sprintf('{"properties.Paddock":{"$in":["%s"]}, "stationname":"%s", "properties.type":"%s"}', paddock, property, type)}

  lookfor <- sprintf('{"_id":false, "stationname":true, "properties.asset_id":true, "properties.Paddock":true,
                     "usehist.date":true, "usehist.num":true, "usehist.numbreed":true, "usehist.numgrow_w":true, "usehist.numgrow_uw":true,
                     "cattlehist.date":true, "cattlehist.num":true, "cattlehist.numbreed":true, "cattlehist.numgrow_w":true, "cattlehist.numgrow_uw":true}')

  jan2<- inf$find(query = filterstation, fields=lookfor)


  cattleinfo <- list()

  for(i in 1:length(jan2$properties$asset_id)){

    dailywts <- setNames(data.frame(matrix(ncol = 5, nrow = length(jan2$usehist$date[[i]]))), c("Date", "GroupCount", "BreedingCount", "Growing_WeanedCount", "Growing_UnweanedCount"))
    dailywts$Date <- jan2$usehist$date[[i]]
    dailywts$GroupCount <- jan2$usehist$num[[i]]
    dailywts$BreedingCount <- jan2$usehist$numbreed[[i]]
    dailywts$GrowingWCount <- jan2$usehist$numgrow_w[[i]]
    dailywts$GrowingUWCount <- jan2$usehist$numgrow_uw[[i]]

    dailywts1 <- setNames(data.frame(matrix(ncol = 5, nrow = length(jan2$cattlehist$date[[i]]))), c("Date", "Group", "Breeding",  "Growing_Weaned", "Growing_Unweaned"))
    dailywts1$Date <- jan2$cattlehist$date[[i]]
    dailywts1$Group <- jan2$cattlehist$num[[i]]
    dailywts$GrowingWCount <- jan2$cattlehist$numgrow_w[[i]]
    dailywts$GrowingUWCount <- jan2$cattlehist$numgrow_uw[[i]]

    use <- merge.data.frame(dailywts1, dailywts, by = "Date", all = T)

    if(is.null(start)) {} else {
      if(is.null(end)){use <- use %>% filter(between(as.Date(Date),start-1,Sys.Date()))} else{
        use <- use %>% filter(between(as.Date(as.POSIXct(Date, tz = "Australia/Brisbane"),tz = "Australia/Brisbane"),start,end+1))}}

    cattleinfo[[jan2$properties$asset_id[i]]] <- as.data.frame(use)
  }

  asset <- jan2
  cattleinfo <- list(asset=asset$properties$asset_id, Paddock = asset$properties$Paddock, Property=asset$stationname, DailyUse=cattleinfo)

  return(cattleinfo)

}
