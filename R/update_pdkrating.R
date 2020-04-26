#' Update ALMS, condition, and long term carrying capacity ratings for paddocks in the DataMuster MongoDB database.
#'
#' This function updates ALMS, condition, and long term carrying capacity ratings in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name update_pdkrating
#' @param property the name of the property to search for
#' @param paddock the name of the paddock that the ALMS rating will be applied to in poly_paddname format. This removes all special characters and will reformats paddocks with numeric names
#' @param ALMSrating the ALMS rating of the paddock. Default is 0
#' @param condition the condition rating of the paddock. Default is A
#' @param LTCC_A the long term carrying capacity of the paddock in condition A. Default is 0
#' @param LTCC_B the long term carrying capacity of the paddock in condition B. Default is 0
#' @param LTCC_C the long term carrying capacity of the paddock in condition C. Default is 0
#' @param LTCC_D the long term carrying capacity of the paddock in condition D. Default is 0
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the paddock has been successfully updated.
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @export


update_pdkrating <- function(property, paddock, ALMSrating=NULL, condition=NULL, LTCC_A=NULL, LTCC_B=NULL, LTCC_C=NULL,
                             LTCC_D=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  paddocks <- mongo(collection = "Paddocks", db = "DataMuster", url = pass, verbose = T)
  stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

  # Check that the paddocks exist in the database
  checkpads <- paste(unlist(paddock), collapse = '", "' )

  filterpaddock <- sprintf('{"stationname":"%s", "poly_paddname":{"$in":["%s"]}}', property, checkpads)

  pad <- paddocks$find(query = filterpaddock, fields = '{"_id":true, "geometry":true, "poly_paddname":true, "properties.hectares":true}')

  if(nrow(pad) != length(unique(paddock))) {
    stop('Could not find matching paddock. Please check the spelling and ensure the paddock is registered in the database')}


  # Check that the properties exist in the database

   filterstation <- sprintf('{"stationname":"%s"}', property)
   station <- stations$count(query = filterstation)

   if(station == 0) {
     stop('Could not find matching property. Please check spelling and ensure the property is registered in the database.')}



   # Update information

  for (i in 1:length(paddock)){
    pdk <- sprintf('{"stationname":"%s", "poly_paddname":"%s"}', property, paddock[i])

    if(!(is.na(pdk))){
      banger <- paddocks$find(query = filterpaddock, fields = '{"stationname":true, "poly_paddname":true, "ALMSrating":true, "condition":true, "LTCC_A":true}')

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
      }}}
