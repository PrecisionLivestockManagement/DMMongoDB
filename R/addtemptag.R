#' Add new tag to the TempTag collection
#'
#' This function adds a new user to the MongoDB database. You can only access this function if you have read and write permission
#' @name addtemptag
#' @param RFID the name of the user to be added to the DataMuster MongoDB Altas server, preferably without a space between the first and last name
#' @param date the email address of the user that was used to register via the DataMuster website
#' @param weight the MongoDB user access level, either "user" or "admin"
#' @param Location a list of the properties the user will be able to access in MongoDB. If access is "admin", the user will be provided access to all existing properties in the database
#' @return This function writes directly to the database. A message will appear to indicate the number of documents that have been successfully added to the MongoDB database
#' @author Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


addtemptag <- function(RFID, date, weight, location, test_tag=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    temptags <- mongo(collection = "TempTags", db = "DataMuster", url = pass, verbose = T)

    template <- temptags$find(query = '{"RFID":"xxxxxx"}', fields = '{"_id":false}')

    returnfields <- sprintf('{"RFID":true, "_id":false}')

    checkRFID <- paste(unlist(checkRFID), collapse = '", "' )

    for (i in 1:length(RFID)){

    searchRFID <- sprintf('{"RFID":"%s"}', RFID[i])

    match <- temptags$find(query = searchRFID, fields = returnfields)

    if (nrow(match) == 1){

    print(paste0(RFID[i], " is already registered in the database. Please use the updatetemptag function to add this information to the existing record"))

    }else{

      newdocument <- template
      newdocument$RFID <- RFID
      newdocument$Test_tag <- ifelse(test_tag == "TRUE", "TRUE", "FALSE")
      newdocument$wthist$date <- list(as.POSIXct(date[i]))
      newdocument$wthist$weight <- list(weight[i])
      newdocument$wthist$location <- list (location[i])

      rownames(newdocument)<-c()

      temptags$insert(newdocument)
    }
    }

}
