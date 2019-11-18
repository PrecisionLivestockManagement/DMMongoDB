#' Upload calving data to the DataMuster database
#'
#' This function allows calving data to be uploaded via the DataMuster website
#' @name appcalvingupload
#' @param date a list of the calving dates in date format
#' @param calfID a list of calf management tag number/s
#' @param weight a list of calf birth weights
#' @param sex the calf's sex (male or female)
#' @param cowID a list of cow management tag number/s
#' @param brand the cow's brand
#' @param udder the cow's udder score
#' @param frontteats the cow's front teat score
#' @param rearteats the cow's rear teat score
#' @param comment observations made by the stationhand as a character entry
#' @param paddock the name of the calving paddock
#' @param property the name of the property
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates the data has been successfully uploaded
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @export


appcalvingupload <- function(calfID, cowID, date, weight, sex, brand, udder, frontteats, rearteats, comment, paddock, property, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  calvingdata <- mongo(collection = "appcalving", db = "DMIoT", url = pass, verbose = T)

  weight <- ifelse(weight == "", 0, weight)
  cowID <- gsub("-", "/", cowID) #All tags in the database are recorded with a / instead of a -

  # Check dates are in the correct format

  datepattern <- "[0-9]{2}/[0-9]{2}/[0-9]{4}"

  checkdates <- grepl(datepattern,date)

  if ("FALSE" %in% checkdates){
    n <- which(checkdates == "FALSE")
    problemdates <- date[n]

    stop("The following Date/s are not in the correct format: ", paste(unlist(problemdates), collapse='     '))}else{
    date <- as.Date(date, format = "%d/%m/%Y")
  }


  # Find cows in the database

  checkIDS <- cowID[cowID != ""]
  checkcows <- paste(unlist(checkIDS), collapse = '", "' )

    filtercattle <- sprintf('{"properties.Management":{"$in":["%s"]}}', checkcows)
    cows <- cattle$find(query = filtercattle, fields = '{"properties.Management":true, "stationname":true, "_id":false}')

    cows <- cows%>%filter(stationname == property)

    if (nrow(cows) < length(checkIDS)) {

      problemcowtags <- as.character(checkIDS[!(checkIDS %in% cows$properties$Management)])

    if (length(problemcowtags) != 0){ #Indicates they are not in the database

      stop("The following Cow ID number/s cannot be found in the database: ", paste(unlist(problemcowtags), collapse='     '))}else{ #Indicates there may be duplicated tags, need to check for twins

        duplicatedcowtags <- as.character(checkIDS[which(duplicated(checkIDS))])

        reportduplicatedcowtags <- c()

        for (i in 1:length(duplicatedcowtags)){

          n <- which(cowID == duplicatedcowtags[i])

          checkcalfdates <- date[n]

          if(length(unique(checkcalfdates))>1){

            reportduplicatedcowtags <- c(reportduplicatedcowtags, duplicatedcowtags[i])
          }}

        if (length(reportduplicatedcowtags) != 0){ #Indicates they are not twins and may be an error

          stop("The following Cow ID number/s have been assigned to more than one calving event : ", paste(unlist(reportduplicatedcowtags), collapse='     '))}

      }}else{

  data <- sprintf('{"Date":{"$date":"%s"}, "Calf ID":"%s", "Weight":%s, "Sex":"%s", "Cow ID":"%s", "Brand":"%s", "Udder":"%s", "Front teats":"%s", "Rear teats":"%s", "Comment":"%s", "Paddock":"%s", "stationname":"%s", "createdAt":{"$date":"%s"}}',
                  paste0(substr(date,1,10),"T","00:00:00","+1000"), calfID, weight, sex, cowID, brand, udder, frontteats, rearteats, comment, paddock, property, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  calvingdata$insert(data)}

}
