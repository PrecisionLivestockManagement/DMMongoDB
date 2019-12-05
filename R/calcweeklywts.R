#' Retrieve data on cattle ALMS weekly weights from the DataMuster database
#'
#' This function calculates average weekly weights from daily ALMS weights for individual or groups of cattle. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name calcweeklywts
#' @param RFID a list of cattle RFID numbers
#' @param start a start date to be returned in date format.
#' @param end an end date to be returned in date format.
#' @param values the minimum number of daily weight values required to calculate an average weekly weight, default is 4
#' @param s.d the minimin standard deviation between daily weight values required to calculate an average weekly weight, default is 25
#' @param remove.duplicates TRUE if duplicate date time values are to removed from the weekly weight calculation or FALSE if all values are to be included, default is TRUE
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe of cattle RFID numbers and weekly weight statistics
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import outliers
#' @export


calcweeklywts <- function(RFID, start=NULL, end=NULL, values=NULL, s.d=NULL, remove.duplicates=NULL, username=NULL, password=NULL){


  if(is.null(username) || is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

  if(is.null(values)){values <- 4}
  if(is.null(s.d)){s.d <- 25}
  if(is.null(remove.duplicates)){remove.duplicates <- "TRUE"}
  if(is.null(start)) {start <- as.Date("2015-01-01")}
  if(is.null(end)) {end <- Sys.Date()}

  getdates <- seq(as.Date(paste0(start)), as.Date(paste0(end)), by = "day")
  getMondays <- getdates[weekdays(getdates) == "Monday"]

  cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

  RFID <- paste(unlist(RFID), collapse = '", "' )

  tempwts <- dailywtsNEW(RFID, start, end)

  filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
  jan <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "pdkhist.dateIN":true, "pdkhist.name":true, "_id":false}')


  cattleinfo <- list()

    if(length(tempwts$RFID) == 0) {} else{

      for(k in 1:length(tempwts$RFID)){

      newdata <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("Date", "avweight", "sdweights", "numweights","paddock"))
      pdhist <- jan$pdkhist[jan$RFID == tempwts$RFID[k],]

        for (l in 1:length(getMondays)){

          start1 <- getMondays[l]
          end1 <- start1 + 6

          if (end1 < end) {

        wts <- tempwts$DailyWeights[[k]] %>% filter(between(as.Date(datetime, tz = "Australia/Brisbane"), start1, end1))

        wts <- wts%>%
               filter(Wt != 0)

        if(remove.duplicates == "FALSE") {stuck <- wts$Wt} else {
          stuck <- wts$Wt[!duplicated(wts$datetime)]}

        for(i in 1:length(stuck)){if(length(stuck) < values){break}else{
          if(sd(stuck) > s.d){stuck <- rm.outlier(stuck)}else
            if(length(stuck) < values){break}else{
              tup1 <- sd(stuck)
              tup2 <- mean(stuck)
              tup3 <- length(stuck)}}}
        if(exists("tup1")){

          data <- data.frame(Date = as.character(end1), avweight = round(tup2,1), sdweights = round(tup1,1), numweights = tup3)

          n <- tail(which(as.Date(pdhist$dateIN[[1]]) < as.Date(data$Date)),1)
          data$paddock <- ifelse(length(n) ==1, pdhist$name[[1]][n], NA)
          #data$paddock <- pdhist$name[[1]][n]

          newdata <- rbind(newdata,data)

          rm(tup1, tup2, tup3)
        } else {}

          }}

      cattleinfo[[tempwts$RFID[k]]] <- as.data.frame(newdata)

    }
    }

  cattleinfo <- list(RFID=tempwts$RFID, Property = tempwts$Property, Paddock = tempwts$Paddock, Weeklyweights=cattleinfo)

  return(cattleinfo)

}
