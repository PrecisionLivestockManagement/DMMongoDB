#' Update cow calving information to DataMuster MongoDB database.
#'
#' This function updates individual or groups of cow calving information in the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_foetalagedata
#' @param RFID a list of cattle RFID number/s
#' @param foetalagedate the date of scanning in date format
#' @param foetalage the foetal age at scanning
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


add_foetalagedata <- function(RFID, foetalagedate, foetalage, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
    calvingdata <- mongo(collection = "CalvingData", db = "DataMuster", url = pass, verbose = T)

    estcalvingdate <- (40 - foetalage)
    estcalvingdate <- foetalagedate + estcalvingdate*7

# Find cows in the database

      checkcows <- paste(unlist(RFID), collapse = '", "' )

      filtercattle <- sprintf('{"RFID":{"$in":["%s"]}}', checkcows)
      cows <- cattle$find(query = filtercattle, fields = '{"RFID":true, "properties.Management":true, "stationname":true, "_id":true}')

      if (nrow(cows) < length(RFID)) {

        problemcows <- as.character(RFID[!(RFID %in% cows$RFID)])

        if (length(problemcows) != 0){ #Indicates they are not in the database

          stop(paste0("The following RFID numbers cannot be found in the database. Please check that the RFID numbers are correct and try again: "), problemcows)}
      }

      cows <- cows %>%
              mutate(MTag = properties$Management) %>%
              select(RFID, MTag, `_id`)


    #  Update CalvingData collection --------------------------

    data <- data.frame(RFID, foetalagedate, foetalage, estcalvingdate, stringsAsFactors = F)
    data <- left_join(data, cows, by = "RFID")

    template <- calvingdata$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')

    template <- template[rep(seq_len(nrow(data)), each = 1), ]

    template <- template %>%
                mutate(RFID = data$RFID,
                       cow_id = data$`_id`,
                       Management = data$MTag,
                       stationname = data$property,
                       foetalagedate = as.POSIXct(paste0(data$foetalagedate,"00:00:00")),
                       foetalage = data$foetalage,
                       estcalvingdate = as.POSIXct(paste0(data$estcalvingdate,"00:00:00")))

    calvingdata$insert(template)


  #  Update animal information in the Cattle collection --------------------------

      for (i in 1:nrow(template)){

        RFIDS <- sprintf('{"RFID":"%s"}', template$RFID[i])


        RFIDI <- sprintf('{"$set":{"preghist.date.%s":{"$date":"%s"}, "preghist.foetalage.%s":%s, "preghist.estcalvingdate.%s":{"$date":"%s"}}}',
                         paste0(substr(template$foetalagedate[i],1,10),"T","00:00:00","+1000"), template$foetalage[i], paste0(substr(template$estcalvingdate[i],1,10),"T","00:00:00","+1000"))

        cattle$update(RFIDS, RFIDI)


      }

    }





