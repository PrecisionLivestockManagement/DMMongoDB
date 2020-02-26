#' Retrieve data on cattle ALMS weekly weights from the DataMuster database
#'
#' This function calculates average weekly weights from daily ALMS weights for individual or groups of cattle. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name calc_weeklywts
#' @param RFID a list of cattle RFID numbers
#' @param start a start date to be returned in date format.
#' @param end an end date to be returned in date format.
#' @param values the minimum number of daily weight values required to calculate an average weekly weight, default is 4
#' @param s.d the minimin standard deviation between daily weight values required to calculate an average weekly weight, default is 25
#' @param minwt the minimum daily weight (kg) required to be included to calculate an average weekly weight, default is 10
#' @param location the filename of the ALMS unit to search for
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe of cattle RFID numbers and weekly weight statistics
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @import outliers
#' @export


calc_weeklywts <- function(RFID=NULL, start=NULL, end=NULL, values=NULL, s.d=NULL, minwt = NULL, location=NULL, username=NULL, password=NULL){

  if(is.null(username) || is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  if(is.null(values)){values <- 4}
  if(is.null(s.d)){s.d <- 25}
  if(is.null(minwt)){minwt <- 10}

  if(is.null(start)) {start <- as.Date("2000-01-01")}
  if(is.null(end)) {end <- Sys.Date()}

  # The weekly weights are calculated each Sunday AEST and uses data from the previous Monday.
  # This section finds all Sunday's between the start and end search dates and then find all previous Monday's
  # New start and end dates are created based on the first Monday and the last Sunday

  dates <- seq(as.Date(paste0(start)), as.Date(paste0(end)), by = "day")
  Sundays <- dates[weekdays(dates) == "Sunday"]
  Mondays <- Sundays-6

  newstart <- Mondays[1]
  newend <- Sundays[length(Sundays)]

  # Retreive the daily weights from the dailywts collection using the new start and end dates

  tempwts <- get_dailywts(RFID = RFID, start = newstart, end = newend, minwt = minwt, location = location,
                          fields = c("RFID", "Wt", "datetime", "Location"),
                          username = username, password = password)

  if(is.null(RFID)){RFID <- unique(tempwts$RFID)}

  # Calculate the weekly weights from the daily weights

  cattleinfo <- list()

   if(nrow(tempwts) == 0) {} else{

      for(k in 1:length(RFID)){

        rfid <- RFID[k]

        newdata <- data.frame()

        #newdata <- setNames(data.frame(matrix(nrow = 0, ncol = 5)), c("Date", "Weight", "sdweights", "numweights","location"), stringsAsFactors = F)

        for (l in 1:length(Sundays)){

          start1 <- Mondays[l]
          end1 <- Sundays[l]

          wts <- tempwts %>%
                 filter(RFID == rfid,
                 between(as.Date(Date, tz = "Australia/Brisbane"), start1, end1))

          if(nrow(wts) == 0) {} else{

            loc <- unique(wts$Location)

            if(length(loc) > 1){cat(paste0(rfid, loc," "))} #This line is just for testing so I know when two locations have been picked up

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

                rm(tup1, tup2, tup3)}
              }
          }
          }

      if(nrow(newdata) == 0){RFID[k] <- "xxxx"}else{

      cattleinfo[[RFID[k]]] <- as.data.frame(newdata)}
      }

    }

  cattleinfo <- list(RFID=RFID[RFID != "xxxx"], WeeklyWeights=cattleinfo)

  return(cattleinfo)

}
