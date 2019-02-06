# This function establishes the credentials and stores them on the local machine to provide ongoing access to the DMMongoDB
# Please check with Lauren to get login details and make sure you run this function before any others as it will be essential
# to providing detailed access.

#' @param user this is the username as a text string that you have been allocated to all access to the DataMuster MongoDB Atlas server
#' @param password this is the password to be able to acces the MongoDB
#' @return this establishes username and password credentials on the local machine and avoids this being
#' @import keyring
#' @export

dmaccess <- function(user){

  if (nrow(keyring::key_list("DMMongoDB"))!=0){
    username = keyring::key_list("DMMongoDB")[1,2]
    for(i in 1 :nrow(keyring::key_list("DMMongoDB")))
      keyring::key_delete("DMMongoDB", username)

  }


  keyring::key_set(service="DMMongoDB",username = user)





}

