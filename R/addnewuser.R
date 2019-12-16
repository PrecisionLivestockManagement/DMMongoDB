#' Add new user
#'
#' This function adds a new user to the MongoDB database. You can only access this function if you have read and write permission
#' @name addnewuser
#' @param user the name of the user to be added to the DataMuster MongoDB Altas server, preferably without a space between the first and last name
#' @param email the email address of the user that was used to register via the DataMuster website
#' @param access the MongoDB user access level, either "user" or "admin"
#' @param property a list of the properties the user will be able to access in MongoDB. If access is "admin", the user will be provided access to all existing properties in the database
#' @return This function writes directly to the database. A message will appear to indicate the number of documents that have been successfully added to the MongoDB database
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return message to say the user has been successfully added
#' @author Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addnewuser <- function(user, email, access, property=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    users <- mongo(collection = "Users", db = "DataMuster", url = pass, verbose = T)

    template <- users$find(query = '{"loginemail":"info@datamuster.net.au"}', fields = '{"_id":false}')

    template$username <- user
    template$loginemail <- email
    template$accesslevel <- access
    template$stations[[1]] <- list()

    if (template$accesslevel == "user") {
      template$stations <- list(property)}

    rownames(template)<-c()

    users$insert(template)

}
