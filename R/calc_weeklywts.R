#' Calculate average weekly weights from the DataMuster MongoDB database.
#'
#' This function calculates average weekly weights from daily ALMS weights for individual or groups of cattle using the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name calc_weeklywts
#' @param RFID a list of cattle RFID number/s
#' @param start a start date to be returned in date format, default is "2014-09-01"
#' @param end an end date to be returned in date format, default is today's date
#' @param values the minimum number of daily weight values required to calculate an average weekly weight, default is 4
#' @param s.d the minimin standard deviation between daily weight values required to calculate an average weekly weight, default is 25
#' @param minwt the minimum daily weight (kg) required to be included to calculate an average weekly weight, default is 10
#' @param location the filename of the ALMS unit to search for
#' @param currentALMS whether or not cattle are currently allocated to an ALMS unit or not
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe of cattle RFID numbers and weekly weight statistics
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import outliers
#' @export


calc_weeklywts <- function(RFID=NULL, start=NULL, end=NULL, values=NULL, s.d=NULL, minwt = NULL, location=NULL, currentALMS=NULL, username=NULL, password=NULL){

  if(is.null(username) || is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(values)){values <- 4}
  if(is.null(s.d)){s.d <- 25}
  if(is.null(minwt)){minwt <- 10}

  if(is.null(start)) {start <- as.Date("2014-01-01")}
  if(is.null(end)) {end <- Sys.Date()}

  # if(is.null(RFID)){}else{
  # RFID <- paste(unlist(RFID), collapse = '", "' )
  # RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)}

  # The weekly weights are calculated each Sunday AEST and uses data from the previous Monday.
  # This section finds all Sunday's between the start and end search dates and then find all previous Monday's
  # New start and end dates are created based on the first Monday and the last Sunday

  dates <- seq(as.Date(paste0(start)), as.Date(paste0(end)), by = "day")
  Sundays <- dates[weekdays(dates) == "Sunday"]
  Mondays <- Sundays-6

  newstart <- Mondays[1]
  newend <- Sundays[length(Sundays)]
  newdates <- seq(as.Date(paste0(newstart)), as.Date(paste0(newend)), by = "day")

  # pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  # cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)
  #
  # # Set up find query
  #
  # search <- paste0("{", RFID, "}")
  #
  # if(nchar(search)==2){}else{
  #   search <- substr(search, 1 , nchar(search)-2)
  #   search <- paste0(search, "}")}
  #
  # # Set up find fields
  #
  # fields = c("RFID", "stationname", "properties.ALMSasset_id")
  #
  # snif <- sprintf('"%s":true', fields)
  # te <- paste0(snif, collapse = ", ")
  # snappy <- sprintf('{%s, "_id":true}', te)
  #
  # #Query database and format for website display
  #
  # cows <- cattle$find(query = search, fields = snappy)


  units <- get_infrastructure(type = "Walk-over-Weighing Unit", filename = location,
                              fields = c("properties.filename", "properties.dual_unit", "stationname", "properties.asset_id"),
                              username = username, password = password)

  cows <- get_almshistory(RFID = RFID, currentALMS = currentALMS,
                          fields = c("RFID", "cattle_id", "ALMS", "dateON", "dateOFF"), username = username, password = password)

  cows <- cows %>% filter(RFID != "xxx xxxxxxxxxxxx", ALMS %in% units$Asset_id)

  cows1 <- data.frame()

  for (i in 1:nrow(cows)){
    dateON <- cows$dateON[i]
    if(is.na(cows$dateOFF[i])){dateOFF <- Sys.Date()}else{dateOFF <- cows$dateOFF[i]}
    ONdates <- seq(as.Date(paste0(dateON)), as.Date(paste0(dateOFF)), by = "day")

    if("TRUE" %in% (newdates %in% ONdates)){
      cows1 <- rbind(cows1, cows[i,])
    }
    }

  RFID <- cows1$RFID
  ID <- cows1$cattle_id

  #cows <- get_cattle(alms = "TRUE", fields = c("RFID", "stationname", "properties.ALMSasset_id"), username = username, password = password)

  # Retreive the daily weights from the dailywts collection using the new start and end dates

  tempwts <- get_dailywts(cattle_id = ID, start = newstart, end = newend, minwt = minwt, location = location, timezone = "Australia/Brisbane",
                          fields = c("RFID", "cattle_id", "Wt", "datetime", "Location"),
                          username = username, password = password)

  #if(is.null(RFID)){
  #  RFID <- unique(c(unique(tempwts$RFID), cows$RFID[!(cows$RFID %in% tempwts$RFID)]))}


  # Calculate the weekly weights from the daily weights

  cattleinfo <- list()

      for(k in 1:length(ID)){

        rfid <- RFID[k]
        id <- ID[k]

        newdata <- data.frame()

        for (l in 1:length(Sundays)){

          start1 <- Mondays[l]
          end1 <- Sundays[l]

          wts <- tempwts %>%
                 filter(cattle_id == id,
                 between(as.Date(Date, tz = "Australia/Brisbane"), start1, end1))

          if(nrow(wts) == 0) {

            unit <- ifelse(id %in% cows1$cattle_id, cows1$ALMS[cows1$cattle_id == id], "")
            unit1 <- ifelse(unit != "", units$filename[units$Asset_id == unit], "")

          data <- data.frame(Date = as.Date(end1), Weight = 0, sdweights = 0, numweights = 0, location = unit1, stringsAsFactors = F)

          } else{

            loc <- unique(wts$Location)

            #If there are records for more than one unit, the code below checks whether or not to split or merge the data between the units

            if(length(loc) > 1){

              dualunits <- units%>%
                           filter(filename %in% loc,
                                  dual_unit == "TRUE")%>%
                           select(filename)

              cnames <- paste(unlist(dualunits), collapse='/')

              wts <- wts%>%
                     mutate(Location = ifelse(Location %in% dualunits$filename, cnames, Location))

              loc <- unique(wts$Location)}

                for(p in 1:length(loc)){

                  stuck <- wts$Weight[wts$Location == loc[p]]

                  for(i in 1:length(stuck)){if(length(stuck) < values){break}else{
                    if(sd(stuck) > s.d){stuck <- rm.outlier(stuck)}else
                      if(length(stuck) < values){break}else{
                        tup1 <- sd(stuck)
                        tup2 <- mean(stuck)
                        tup3 <- length(stuck)}}}

                  if(exists("tup1")){

                    data <- data.frame(Date = as.Date(end1), Weight = round(tup2,1), sdweights = round(tup1,1), numweights = tup3, location = loc[p], stringsAsFactors = F)

                    newdata <- rbind(newdata, data)

                    rm(tup1, tup2, tup3)}else{

                      unit <- ifelse(id %in% cows1$cattle_id, cows1$ALMS[cows1$cattle_id == id], "")
                      unit1 <- ifelse(unit != "", units$filename[units$Asset_id == unit], "")

                      data <- data.frame(Date = as.Date(end1), Weight = 0, sdweights = 0, numweights = 0, location = loc[p], stringsAsFactors = F)

                    newdata <- rbind(newdata, data)

                    }}
            }
                    #newdata <- rbind(newdata, data)
          }


                   cattleinfo[[RFID[k]]] <- as.data.frame(newdata)
                   }


  cattleinfo <- list(RFID=RFID, WeeklyWeights=cattleinfo)

  return(cattleinfo)

}
