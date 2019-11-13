#' Package with functions to enable easier code to access to DataMuster MongoDB Atlas servers
#'
#' This function allows calving data to be uploaded from the DataMuster website
#' @name appcalvingupload
#' @param date the calving date
#' @param calfID the management tag number assigned to the calf
#' @param weight the birth weight of the calf
#' @param sex the sex of the calf
#' @param cowID the management tag number of the dam
#' @param brand the dam's brand
#' @param udder the udder score of the dam
#' @param frontteats the teat score of the dam's front teats
#' @param rearteats the teat score of the dam's rear teats
#' @param comment observations made by the stationhand as txt
#' @param paddock the paddock the dam calved in
#' @param property the property that the dam calved on
#' @param username you will need to request access from Lauren O'Connor for a username to write data to the database
#' @param password you will need to request access from Lauren O'Connor for a username to write data to the database
#' @return uploads data in a dataframe to the DMMongoDB database
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
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
