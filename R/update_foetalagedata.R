#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in CalvingData collection in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_foetalagedata
#' @param RFID a list of cattle RFID number/s
#' @param date the calving date of the cattle, in date format
#' @param multiples if the cow had twins or not – TRUE or FALSE. Default is FALSE
#' @param algdev if the cow was used for the calving algorithm development - TRUE or FALSE. Default is FALSE
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


update_foetalagedata <- function(RFID, date = NULL, multiples = NULL, algdev = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  calvingdata <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)

  # Find cows in the database

  cows <- get_cattle(RFID = RFID, username = username, password = password)

  if (nrow(cows) < length(RFID)) {

    problemcows <- as.character(RFID[!(RFID %in% cows$RFID)])

    if (length(problemcows) != 0){ #Indicates they are not in the database

      stop(paste0("The following RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again: "), problemcows)}
  }

  #  Update animal information in Cattle collection --------------------------

  for (i in 1:length(RFID)){

    IDS <- sprintf('{"RFID":"%s"}', RFID[i])

    #banger <- cattle$find(query= IDS, fields='{"properties.calvingdate":true}')
    #arrpos <- length(banger$properties$calvingdate[[1]])

    #matchdate <- which(substr(banger$properties$calvingdate[[1]],1,7) == substr(date[i],1,7))

    #if (length(matchdate) == 0){

      RFIDI <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(date[i],1,10),"T","00:00:00","+1000"))
      cattle$update(IDS, RFIDI)

    #}
      }

  # Update animal information in CalvingData collection


  for (i in 1:length(RFID)){

    #cow <- cows[cows$RFID == RFID[i],]

    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])

    #if(!is.null(RFID)){

      template <- calvingdata$find(query = RFIDS, fields = '{"RFID":true, "calvingdate":true, "multiples":true, "DoBalgdev":true, "foetalagedate":true}')

      #if(length(template)>1){
        if(nrow(template)>1){
        template <- template %>% group_by(RFID) %>% slice(n()) %>% ungroup()
        # This is selecting the last record. I think we need a better identifier as to which record we want e.g. a calving season field
        }

      if(is.null(multiples)){multiples = FALSE}
      if(is.null(algdev)){algdev = FALSE}

      if(length(date) == 0){
        arrpos <- length(template$calvingdate[[1]])
        RFIDI <- sprintf('{$set":{"multiples":"%s", "DoBalgdev":"%s"}}', multiples, algdev)
      }else{
        arrpos <- length(template$calvingdate[[1]])
        RFIDI <- sprintf('{"$set":{"calvingdate":{"$date":"%s"}, "multiples":"%s", "DoBalgdev":"%s"}}', paste0(substr(date,1,10),"T","00:00:00","+1000"),
                         multiples, algdev)

      }
      calvingdata$update(RFIDS, RFIDI)
      #}
  }
}






