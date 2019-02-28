#' Add new user
#'
#' This function adds a new user to the MongoDB database. You can only access this function if you have read and write permission
#' @name newuser
<<<<<<< HEAD
#' @param user the name of the user to be added to the DataMuster MongoDB Altas server, preferably without a space between the first and last name
#' @param email the email address of the user that was used to register via the DataMuster website
#' @param access the MongoDB user access level, either "user" or "admin"
#' @param property a list of the properties the user will be able to access in MongoDB. If access is "admin", the user will be provided access to all existing properties in the database
#' @return This function writes directly to the database. A message will appear to indicate the number of documents that have been successfully added to the MongoDB database
=======
#' @param user this is the user name
#' @param email provide a user email address
#' @param access provide list of properties that they can access
#' @param property name of the property that the user is associated with
#' @return message to say the user has been successfull added
>>>>>>> d2e0bb61963201508d9f6b5791c8ce0deda1b371
#' @author Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


newuser <- function(user, email, access, property){


    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    users <- mongo(collection = "Users", db = "DataMuster", url = pass, verbose = T)

    template <- users$find(query = '{"loginemail":"info@datamuster.net.au"}', fields = '{"_id":false}')

    template$username <- user
    template$loginemail <- email
    template$accesslevel <- access

    if (template$accesslevel == "user") {

    i<-1

    for (i in 1:length(property)) {

    template$stations[[1]][i+1]<- property[[i]][1]} } else {

      if (template$accesslevel == "admin") {

        stations <- mongo(collection = "Stations", db = "DataMuster", url = pass, verbose = T)

        props <- stations$find(query = '{}', fields = '{"name":true, "_id":false}')

        props1 <- split(props, col(props))

        for (i in 1:length(props1$`1`$name)) {

          template$stations[[1]][i]<- props1$`1`$name[i]} } }

    rownames(template)<-c()

    users$insert(template)

}
