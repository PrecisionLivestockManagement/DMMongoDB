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
  calvingdata <- mongo(collection = "appcalving", db = "DMIoT", url = pass, verbose = T)

  date <- as.Date(date, format = "%d/%m/%Y")
  weight <- ifelse(weight == "", 0, weight)

  data <- sprintf('{"Date":{"$date":"%s"}, "Calf ID":"%s", "Weight":%s, "Sex":"%s", "Cow ID":"%s", "Brand":"%s", "Udder":"%s", "Front teats":"%s", "Rear teats":"%s", "Comment":"%s", "Paddock":"%s", "stationname":"%s", "createdAt":{"$date":"%s"}}',
                  paste0(substr(date,1,10),"T","00:00:00","+1000"), calfID, weight, sex, cowID, brand, udder, frontteats, rearteats, comment, paddock, property, paste0(substr(Sys.time(),1,10),"T",substr(Sys.time(),12,19),"+1000"))

  calvingdata$insert(data)

}
