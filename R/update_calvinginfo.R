#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_calvinginfo
#' @param RFID a list of cattle RFID number/s
#' @param MTag a list of cattle management tag number/s
#' @param property the name of the property to search for
#' @param calfRFID the calf's RFID number
#' @param calfMTag the calf's management tag number
#' @param birthDate the cow's date of calving in date format
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


update_calvinginfo <- function(MTag, calfMTag, property, birthDate, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

# Check that the property is registered in the database ---------------------------------------------------------------------

    filterstation <- sprintf('{"name":"%s"}', property)
    station <- stations$count(query = filterstation)

    if(station == 0) {
      stop('Could not find matching property. Please check spelling and ensure the property is registered in the database.')}


# Find cows in the database

    if (!(is.null(MTag))){
      checkcows <- paste(unlist(MTag), collapse = '", "' )

      filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcows)
      cows <- cattle$find(query = filtercattle, fields = '{"properties.Management":true, "stationname":true, "_id":false}')

      cows <- cows%>%filter(stationname == property)

      if (nrow(cows) < length(MTag)) {

        problemcowtags <- as.character(MTag[!(MTag %in% cows$properties$Management)])

        if (length(problemcowtags) != 0){ #Indicates they are not in the database

        stop(paste0("The following MTag numbers cannot be found in the database. Please check that the MTag numbers are correct and try again: "), problemcowtags)}
    }}

# Find calves in the database

    if (!(is.null(calfMTag))){

      checkcalves <- paste(unlist(calfMTag), collapse = '", "' )

      filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcalves)
      calves <- cattle$find(query = filtercattle, fields = '{"RFID":true, "properties.Management":true, "stationname":true, "_id":true}')

      calves <- calves%>%filter(stationname == property)

      if (nrow(calves) < length(calfMTag)) {

        problemcalftags <- as.character(calfMTag[!(calfMTag %in% calves$properties$Management)])

        stop(paste0("The following calf MTag numbers cannot be found in the database. Please check that the MTag numbers are correct and try again: "), problemcalftags)}
    }


  #  Update animal information --------------------------

      for (i in 1:length(MTag)){

        calfid <- calves$`_id`[calves$properties$Management == calfMTag[i]]

        IDS <- sprintf('{"stationname":"%s","properties.Management":"%s"}', property, MTag[i])

        banger <- cattle$find(query= IDS, fields='{"calfhist.date":true, "_id":false}')
        arrpos <- length(banger$calfhist$date[[1]])

        matchdate <- which(substr(banger$calfhist$date[[1]],1,7) == substr(birthDate[i],1,7))

        if (length(matchdate) == 0){

          RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[i],1,10),"T","00:00:00","+1000"), arrpos, calfid)
          RFIDIlast <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(birthDate[i],1,10),"T","00:00:00","+1000"))

          cattle$update(IDS, RFIDI)
          cattle$update(IDS, RFIDIlast)

          }}

}





