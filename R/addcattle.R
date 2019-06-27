#' Add cattle to DataMuster database
#'
#' This function adds individual or groups of cattle to the DataMuster database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name addcattle
#' @param RFID this is a list of the cattle RFID numbers
#' @param MTag this is a list of the cattle managemeny tag numbers
#' @param category the class of animal (breeding or growing)
#' @param property the name of the property to add the cattle
#' @param paddock the name of the paddock to add the cattle
#' @param weaned the weaned status of the animal. TRUE if cattle are weaned and FALSE if cattle are not weaned
#' @param date provide the date that the animals were added, this has to be in date format. Default is today's date
#' @param breed the animal's breed
#' @param brand the animal's brand
#' @param horn the animal's horn status
#' @param colour the animal's sex, male or female
#' @param sex the animal's sex, male or female
#' @param desexed the animal's desexed status, TRUE if castrated/spayed, FALSE if entire
#' @param origin the animal's previous location
#' @param DOB the animal's date of bith, this has to be in date format
#' @param birthWeight the animal's weight at bith
#' @param damRFID the dam's RFID number
#' @param damMTag the dam's management tag number
#' @param sireRFID the sire's RFID number
#' @param sireMTag the sire's management tag number
#' @param animalID this is a unique ID number
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addcattle <- function(RFID, MTag, category, property, paddock, weaned, date=NULL, breed=NULL, brand=NULL,
                      horn=NULL, colour=NULL, sex=NULL, desexed=NULL,  origin=NULL, DOB=NULL, birthWeight=NULL,
                      damRFID=NULL, damMTag=NULL, sireRFID=NULL, sireMTag=NULL, animalID=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)
    paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
    infs <- mongo(collection = "Infrastructure", db = "DataMuster", url = pass, verbose = T)
    culls <- mongo(collection = "Culls", db = "DataMuster", url = pass, verbose = T)

    if(is.null(date)){date <- Sys.Date()}else{date <- as.POSIXct(date)}

    if (length(date) == 1){date <- rep(date, length = length(RFID))}



    #  Create template dataframes --------------------------


  tempcattle <- cattle$find(query = '{"RFID":"xxxxxx"}', fields = '{"_id":false}')


mandfields <- c("RFID","MTag","category","paddock","weaned","date") #excluding property

optfields <- c("breed", "brand","horn","colour","sex","desexed","origin","DOB","birthWeight","damRFID",
                "damMTag","sireRFID","sireMTag","animalID")

i<-1
for (i in 1:length(mandfields)){
  if (length(get(mandfields[i])) == 1){assign(mandfields[i],rep(get(mandfields[i]), length = length(RFID)))}}

i<-1
for (i in 1:length(optfields)){
  if (length(get(optfields[[i]])) == 1){assign(optfields[i],rep(get(optfields[i]), length = length(RFID)))}}


# Check that the RFID numbers are in the correct format and are not already registered in the database ---------------------------------------------------------------------


    if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
      stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

    checkRFID <- RFID[which(RFID != "xxx xxxxxxxxxxxx")]
    checkRFID <- paste(unlist(checkRFID), collapse = '", "' )

    #filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkRFID)
    # check <- cattle$count(query = filtercattle)
    #
    # if (check != 0) {
    #
    #   stop("One or more of the RFID numbers are already registered in the database. Please check that the RFID numbers are correct and try again")}
    #
    # check1 <- culls$count(query = filtercattle)
    #
    # if (check1 != 0) {
    #
    #   stop("One or more of the RFID numbers are registered as a cull in the database. Please check that the RFID numbers are correct and try again")}


  # Check that any sire or dam RFID numbers are in the correct format ---------------------------------------------------------------------

    if (!(is.null(damRFID))){
    if("TRUE" %in% (nchar(as.character(damRFID))!= 16)) {
      stop(paste0("One or more of the dam RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}}

    if (!(is.null(sireRFID))){
    if("TRUE" %in% (nchar(as.character(sireRFID))!= 16)) {
      stop(paste0("One or more of the dam RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}}


    # Check that the property is registered in the database ---------------------------------------------------------------------

    filterstation <- sprintf('{"name":"%s"}', property)
    station <- stations$find(query = filterstation, fields = '{"_id":true}')

    if(nrow(station) == 0) {
            stop('Could not find matching property. Please check spelling and ensure the property is registered in the database.')}


    # Check that the paddocks are registered in the database ---------------------------------------------------------------------


    checkpads <- paste(unlist(paddock), collapse = '", "' )

    filterpaddock <- sprintf('{"stationname":"%s", "paddname":{"$in":["%s"]}}', property, checkpads)

    pad <- paddocks$find(query = filterpaddock, fields = '{"_id":true, "geometry":true, "paddname":true, "properties.hectares":true}')

    if(nrow(pad) != length(unique(paddock))) {
      stop('Could not find matching paddock. Please check the spelling and ensure the paddock is registered in the database')
    }

    # Check for WoW infrastructure in assigned paddocks

    filterinfs <- sprintf('{"stationname":"%s", "properties.Paddock":{"$in":["%s"]}, "properties.type":"%s"}', property, checkpads, "Walk-over-Weighing Unit")

    inf <- infs$find(query = filterinfs, fields = '{"_id":true, "properties.Paddock":true, "properties.asset_id":true}')


  #  Insert animal information and upload to the database --------------------------

    p<-1
    for (p in 1:length(RFID)){

      filtercattle <- sprintf('{"RFID":"%s"}', RFID[p])
      check <- cattle$find(query = filtercattle, fields = '{"_id":false, "RFID":true}')

      if (nrow(check) == 1){
        print(paste0(RFID[p], " is already registered in the database. This animal has not been added"))}  else {

      template <- cattle$find(query = '{"RFID":"xxxxxx"}', fields = '{"_id":false}')

    # Mandatory information

      #Identification, category & weaning information
      template$RFID <- RFID[p]
      template$RFIDhist$date <- list(as.POSIXct(date[p]))
      template$RFIDhist$ID <- list(RFID[p])

      template$properties$Management <- as.character(MTag[p])
      template$properties$category <- category[p]
      template$properties$weaned <- weaned[p]

      # Property information
      template$stationname <- property
      template$stationID<- station$`_id`
      template$properties$entryDate <- as.POSIXct(date[p])

      # Paddock information
      temppad <- pad[which(pad$paddname == paddock[p]),]
      template$properties$Paddock<-paddock[p]
      template$properties$PaddockID<- temppad$`_id`
      template$pdkhist$name <- list(paddock[p])
      template$pdkhist$ID <- list(temppad$`_id`)
      template$pdkhist$dateIN <- list(as.POSIXct(date[p]))

      #ALMS information
      if (temppad$paddname %in% inf$properties$Paddock){
        tempinf <- inf[which(inf$properties$Paddock == temppad$paddname),]
        template$properties$ALMS <- "TRUE"
        template$properties$ALMSID<- tempinf$`_id`
        template$properties$ALMSasset_id<- tempinf$properties$asset_id
        template$properties$ALMSdateON <- as.POSIXct(date[p])}

    # Optional information

      if (!(is.null(breed))){template$properties$breed <- breed[p]}
      if (!(is.null(brand))){template$properties$brand <- brand[p]}
      if (!(is.null(horn))){template$properties$horn <- tolower(horn[p])}
      if (!(is.null(colour))){template$properties$colour <- tolower(colour[p])}
      if (!(is.null(sex))){template$properties$sex <- tolower(sex[p])}
      if (!(is.null(desexed))){template$properties$desexed <- desexed[p]}
      if (!(is.null(origin))){template$properties$origin <- origin[p]}
      if (!(is.null(DOB))){template$properties$birthDate <- as.POSIXct(DOB[p])}
      if (!(is.null(birthWeight))){template$properties$birthWeight <- birthWeight[p]}
      if (!(is.null(animalID))){template$properties$animalID <- animalID[p]}

      if (!(is.null(damRFID))){

        if (damRFID[p] != ""){

        template$properties$damRFID <- damRFID[p]

         filterdam <- sprintf('{"RFID":"%s"}', damRFID[p])
          dam <- cattle$find(query = filterdam, fields = '{"_id":true,"properties.Management":true}')

          if (nrow(dam) == 0){
            print(paste0('The dam RFID ', damRFID[p], ' is not registered in the database. The dam RFID has been noted but is not linked in the database'))}else{

           template$properties$damID <- dam$`_id`
            template$properties$damMTag <- dam$properties$Management}}}

      if (!(is.null(sireRFID))){

        if (sireRFID[p] != ""){

        template$properties$sireRFID <- sireRFID[p]

        filtersire <- sprintf('{"RFID":"%s"}', sireRFID[p])
        sire <- cattle$find(query = filtersire, fields = '{"_id":true,"properties.Management":true}')

        if (nrow(sire) == 0){
          print(paste0('The sire RFID ', sireRFID[p], ' is not registered in the database. The sire RFID has been noted but is not linked in the database'))}else{

            template$properties$sireID <- sire$`_id`
            template$properties$sireMTag <- sire$properties$Management}}}

      if (!(is.null(damMTag))){
        if (damMTag[p] != ""){

        if (template$properties$damMTag == "xxxxxx"){
        template$properties$damMTag <- as.character(damMTag[p])

        filterdam <- sprintf('{"properties.Management":"%s"}', damMTag[p])
        dam <- cattle$find(query = filterdam, fields = '{"_id":true, "RFID":true}')

        if (nrow(dam) == 0){
          print(paste0('The dam MTag ', damMTag[p], ' is not registered in the database. The dam MTag has been noted but is not linked in the database'))}else{

          template$properties$damID <- dam$`_id`
          template$properties$damRFID <- dam$RFID}}}}

      if (!(is.null(sireMTag))){
        if (sireMTag[p] != ""){

        if (template$properties$sireMTag == "xxxxxx"){
          template$properties$sireMTag <- as.character(sireMTag[p])

          filtersire <- sprintf('{"properties.Management":"%s"}', sireMTag[p])
          sire <- cattle$find(query = filtersire, fields = '{"_id":true, "RFID":true}')

          if (nrow(sire) == 0){
            print(paste0('The sire MTag ', sireMTag[p], ' is not registered in the database. The sire MTag has been noted but is not linked in the database'))}else{

              template$properties$sireID <- sire$`_id`
              template$properties$sireRFID <- sire$RFID}}}}


    #  Fill any missing fields with default values --------------------------


    for (n in 1:length(template[2][[1]])){

      if(as.character(template[2][[1]][n][]) == "" || as.character(template[2][[1]][n][]) == "null") {template[2][[1]][n][] <- tempcattle[2][[1]][[n]][]}}

  cattle$insert(template)

    }}

movecattle(property = property, paddock = unique(paddock), username = username, password = password)

    }



