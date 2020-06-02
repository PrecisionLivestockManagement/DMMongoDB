#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_calvingdata
#' @param RFID a list of cattle management RFID/s
#' @param MTag a list of cattle management tag number/s
#' @param property the name of the property
#' @param birthDate the cow's date of calving in date format
#' @param foetalagedate the date of scanning in date format
#' @param foetalage the foetal age at scanning
#' @param estcalvingdate the estimated date of calving at scanning in date format
#' @param multiples if the calf is a twin or single, TRUE or FALSE
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


update_calvingdata <- function(RFID = NULL, MTag = NULL, property = NULL, foetalage = NULL, foetalagedate = NULL, estcalvingdate = NULL, birthDate = NULL, multiples = NULL, DoBalgdev = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

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


    if(!is.null(RFID)){
      for(i in 1:length(RFID)){
        RFID <- cows$`_id`[cows$RFID == RFID[i]]
        IDS <- sprintf('{"stationname":"%s", "RFID":"%s"}', property, RFID[i])
        banger <- cattle$find(query = IDS, fields = '{"calvingdate":true, "_id":false}')
        arrpos <- length(banger$calvingdate[[1]])
      }
    }

    for (i in 1:length(RFID)){


      cowid <- cows$`_id`[cows$properties$Management == MTag[i]]

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







   for (i in 1:length(RFID)){
      cows <- sprintf('{"stationname":"%s", "poly_paddname":"%s"}', property, RFID[i])

      if(!(is.na(pdk))){
        banger <- paddocks$find(query = filterpaddock, fields = '{"stationname":true, "poly_paddname":true, "ALMSrating":true, "condition":true, "LTCC_A":true}')
      }}





    if(!(is.null(foetalage))){
      if(!(is.na(foetalage[i]))){
        if(foetalage[i] != ""){
          RFIDI <- sprintf('{"$set":{"foetalage":"%s"}}', foetalage[i])
          cattle$update()
        }
      }
    }





      # ALMSrating
      if(!(is.null(ALMSrating))){
        if(!(is.na(ALMSrating[i]))){
          if(ALMSrating[i] != ""){
            RFIDI <- sprintf('{"$set":{"ALMSrating":"%s"}}', ALMSrating[i])
            paddocks$update(pdk, RFIDI)}}}
    # condition
    if(!(is.null(condition))){
      if(!(is.na(condition[i]))){
        if(condition[i] != ""){
          RFIDI <- sprintf('{"$set":{"condition":"%s"}}', condition[i])
          paddocks$update(pdk, RFIDI)}}}
    # LTCC
    if(!(is.null(LTCC_A))){
      if(!(is.na(LTCC_A[i]))){
        if(LTCC_A[i] != ""){
          RFIDI <- sprintf('{"$set":{"LTCC_A":"%s"}}', LTCC_A[i])
          paddocks$update(pdk, RFIDI)}}}
    # LTCC_B
    if(!(is.null(LTCC_B))){
      if(!(is.na(LTCC_B[i]))){
        if(LTCC_B[i] != ""){
          RFIDI <- sprintf('{"$set":{"LTCC_B":"%s"}}', LTCC_B[i])
          paddocks$update(pdk, RFIDI)}}}
    # LTCC_C
    if(!(is.null(LTCC_C))){
      if(!(is.na(LTCC_C[i]))){
        if(LTCC_C[i] != ""){
          RFIDI <- sprintf('{"$set":{"LTCC_C":"%s"}}', LTCC_C[i])
          paddocks$update(pdk, RFIDI)}}}
    # LTCC_D
    if(!(is.null(LTCC_D))){
      if(!(is.na(LTCC_D[i]))){
        if(LTCC_D[i] != ""){
          RFIDI <- sprintf('{"$set":{"LTCC_D":"%s"}}', LTCC_D[i])
          paddocks$update(pdk, RFIDI)}}}




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


    #  Update CalvingData collection --------------------------

    for (i in 1:length(MTag)){

      calfid <- calves$`_id`[calves$properties$Management == calfMTag[i]]

      IDS <- sprintf('{"stationname":"%s","properties.Management":"%s"}', property, MTag[i])

      banger <- cattle$find(query= IDS, fields='{"calfhist.date":true, "_id":false}')
      arrpos <- length(banger$calfhist$date[[1]])

      matchdate <- which(substr(banger$calfhist$date[[1]],1,7) == substr(birthDate[i],1,7))

      if (length(matchdate) == 0){

        #RFIDI <- sprintf('{"$set":{"calfhist.date.%s":{"$date":"%s"}, "calfhist.ID.%s":"%s"}}', arrpos, paste0(substr(birthDate[i],1,10),"T","00:00:00","+1000"), arrpos, calfid)
        RFIDIlast <- sprintf('{"$set":{"properties.calvingdate":{"$date":"%s"}}}', paste0(substr(birthDate[i],1,10),"T","00:00:00","+1000"))

        #cattle$update(IDS, RFIDI)
        cattle$update(IDS, RFIDIlast)

      }}

  #  Update animal information in the Cattle collection --------------------------

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





