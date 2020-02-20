#' Update cattle information to DataMuster database
#'
#' This function updates individual or groups of cattle information to the DataMuster database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name updatecattleinfo
#' @param RFID this is a list of the cattle RFID numbers
#' @param MTag this is a list of the cattle management tag numbers
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
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the RFID tag number has been successfully updated
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


updatecattleinfo <- function(RFID, MTag=NULL, category=NULL, weaned=NULL, breed=NULL, brand=NULL, rego=NULL,
                      horn=NULL, colour=NULL, sex=NULL, desexed=NULL,  origin=NULL, DOB=NULL, birthWeight=NULL,
                      damRFID=NULL, damMTag=NULL, sireRFID=NULL, sireMTag=NULL, username=NULL, password=NULL){

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

    #  Create template dataframes --------------------------

fields <- c("MTag","category","weaned","breed", "brand", "rego","horn","colour","sex","desexed","origin","DOB","birthWeight","damRFID",
                "damMTag","sireRFID","sireMTag") #excluding RFID & property

i<-1
for (i in 1:length(fields)){
  if (length(get(fields[i])) == 1){assign(fields[i],rep(get(fields[i]), length = length(RFID)))}}


# Check that the RFID numbers are in the correct format and exist in the database

if("TRUE" %in% (nchar(as.character(RFID))!= 16)) {
  stop(paste0("One or more of the RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}

checkcows <- paste(unlist(RFID), collapse = '", "' )

filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
check <- cattle$count(query = filtercattle)

if (check != length(RFID)) {
  stop("One or more of the RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again")}


  # Check that any sire or dam RFID numbers are in the correct format ---------------------------------------------------------------------

    if (!(is.null(damRFID))){
    if("TRUE" %in% (nchar(as.character(damRFID))!= 16)) {
      stop(paste0("One or more of the dam RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}}

    if (!(is.null(sireRFID))){
    if("TRUE" %in% (nchar(as.character(sireRFID))!= 16)) {
      stop(paste0("One or more of the dam RFID numbers are not in the correct format. Please ensure all RFIDs are in the format 'xxx xxxxxxxxxxxx'"))}}


  #  Update animal information --------------------------

    p<-1
    for (p in 1:length(RFID)){

      RFIDS <- sprintf('{"RFID":"%s"}', RFID[p])

      #MTag
      if (!(is.null(MTag))){
        if (!(is.na(MTag[p]))){
        if (MTag[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.Management":"%s"}}', MTag[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Category
      if (!(is.null(category))){
        if (!(is.na(category[p]))){
        if (category[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.category":"%s"}}', category[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Weaned
      if (!(is.null(weaned))){
        if (!(is.na(weaned[p]))){
        if (weaned[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.weaned":"%s"}}', weaned[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Breed
      if (!(is.null(breed))){
        if (!(is.na(breed[p]))){
        if (breed[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.breed":"%s"}}', breed[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Brand
      if (!(is.null(brand))){
        if (!(is.na(brand[p]))){
        if (brand[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.brand":"%s"}}', brand[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Rego
      if (!(is.null(rego))){
        if (!(is.na(rego[p]))){
          if (rego[p] != ""){
            RFIDI <- sprintf('{"$set":{"properties.rego":"%s"}}', rego[p])
            cattle$update(RFIDS, RFIDI)}}}
      #Horn
      if (!(is.null(horn))){
        if (!(is.na(horn[p]))){
        if (horn[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.horn":"%s"}}', tolower(horn[p]))
          cattle$update(RFIDS, RFIDI)}}}
      #Colour
      if (!(is.null(colour))){
        if (!(is.na(colour[p]))){
        if (colour[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.colour":"%s"}}', tolower(colour[p]))
          cattle$update(RFIDS, RFIDI)}}}
      #Sex
      if (!(is.null(sex))){
        if (!(is.na(sex[p]))){
        if (sex[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.sex":"%s"}}', tolower(sex[p]))
          cattle$update(RFIDS, RFIDI)}}}
      #Desexed
      if (!(is.null(desexed))){
        if (!(is.na(desexed[p]))){
        if (desexed[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.desexed":"%s"}}', desexed[p])
          cattle$update(RFIDS, RFIDI)}}}
      #Origin
      if (!(is.null(origin))){
        if (!(is.na(origin[p]))){
        if (origin[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.origin":"%s"}}', origin[p])
          cattle$update(RFIDS, RFIDI)}}}
      #DOB
      if (!(is.null(DOB))){
        if (!(is.na(DOB[p]))){
          if (as.character(DOB[p]) != ""){
          RFIDI <- sprintf('{"$set":{"properties.birthDate":{"$date":"%s"}}}', paste0(substr(DOB[p],1,10),"T","00:00:00","+1000"))
          cattle$update(RFIDS, RFIDI)}}}
      #birthWeight
      if (!(is.null(birthWeight))){
        if (!(is.na(birthWeight[p]))){
        if (birthWeight[p] != ""){
          RFIDI <- sprintf('{"$set":{"properties.birthWeight":"%s"}}', birthWeight[p])
          cattle$update(RFIDS, RFIDI)}}}

#Dam RFID
if (!(is.null(damRFID))){
  if (!(is.na(damRFID[p]))){
  if (damRFID[p] != ""){

    filterdam <- sprintf('{"RFID":"%s"}', damRFID[p])
    dam <- cattle$find(query = filterdam, fields = '{"_id":true,"properties.Management":true}')

    if (nrow(dam) == 0){
      RFIDI <- sprintf('{"$set":{"properties.damRFID":"%s"}}', damRFID[p])
      print(paste0('The dam RFID ', damRFID[p], ' is not registered in the database. The dam RFID has been noted but is not linked in the database'))}else{

        RFIDI <- sprintf('{"$set":{"properties.damRFID":"%s", "properties.damID":"%s", "properties.damMTag":"%s"}}', damRFID[p], dam$`_id`, dam$properties$Management)}

    cattle$update(RFIDS, RFIDI)}}}

#Dam Management
if (!(is.null(damMTag))){
  if (!(is.na(damMTag[p]))){
  if (damMTag[p] != ""){

    tempcow <- cattle$find(query = RFIDS, fields = '{"_id":false,"properties.damMTag":true}')

    if (tempcow$properties$damMTag == "xxxxxx"){

      filterdam <- sprintf('{"properties.Management":"%s"}', damMTag[p])
      dam <- cattle$find(query = filterdam, fields = '{"_id":true, "RFID":true}')

        if (nrow(dam) == 0){
          RFIDI <- sprintf('{"$set":{"properties.damMTag":"%s"}}', damMTag[p])
        print(paste0('The dam MTag ', damMTag[p], ' is not registered in the database. The dam MTag has been noted but is not linked in the database'))}else{
          RFIDI <- sprintf('{"$set":{"properties.damRFID":"%s", "properties.damID":"%s", "properties.damMTag":"%s"}}', dam$RFID, dam$`_id`, damMTag[p])}

      cattle$update(RFIDS, RFIDI)}}}}

#Sire RFID
  if (!(is.null(sireRFID))){
    if (!(is.na(sireRFID[p]))){

    if (sireRFID[p] != ""){

      filtersire <- sprintf('{"RFID":"%s"}', sireRFID[p])
      sire <- cattle$find(query = filtersire, fields = '{"_id":true,"properties.Management":true}')

      if (nrow(sire) == 0){
        RFIDI <- sprintf('{"$set":{"properties.sireRFID":"%s"}}', sireRFID[p])
        print(paste0('The sire RFID ', sireRFID[p], ' is not registered in the database. The sire RFID has been noted but is not linked in the database'))}else{

          RFIDI <- sprintf('{"$set":{"properties.sireRFID":"%s", "properties.sireID":"%s", "properties.sireMTag":"%s"}}', sireRFID[p], sire$`_id`, sire$properties$Management)}

      cattle$update(RFIDS, RFIDI)}}}

#Sire Management
  if (!(is.null(sireMTag))){
    if (!(is.na(sireMTag[p]))){
    if (sireMTag[p] != ""){

      tempcow <- cattle$find(query = RFIDS, fields = '{"_id":false,"properties.sireMTag":true}')

      if (tempcow$properties$sireMTag == "xxxxxx"){

        filtersire <- sprintf('{"properties.Management":"%s"}', sireMTag[p])
        sire <- cattle$find(query = filtersire, fields = '{"_id":true, "RFID":true}')

        if (nrow(sire) == 0){
          RFIDI <- sprintf('{"$set":{"properties.sireMTag":"%s"}}', sireMTag[p])
          print(paste0('The sire MTag ', sireMTag[p], ' is not registered in the database. The sire MTag has been noted but is not linked in the database'))}else{
            RFIDI <- sprintf('{"$set":{"properties.sireRFID":"%s", "properties.sireID":"%s", "properties.sireMTag":"%s"}}', sire$RFID, sire$`_id`, sire$MTag[p])}

        cattle$update(RFIDS, RFIDI)}}}}

    }
}



