#' Calculate the calving date using cattle ALMS weights from the DataMuster MongoDB database.
#'
#' This function calculates the calving date of individuals or groups of cattle using the DataMuster MongoDB database. The code first calculates the calving week using weekly weight. The largest negative weight difference between the weekly weights is assigned as the calving week.
#' The calving date is then calculated using daily weight data. The data is filtered for the period around the detected calving week and erroneous weights are removed. The largest negative weight difference between the remaining daily weights is assigned as the calving date. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name calc_calvingdate
#' @param RFID a list of cattle RFID number/s
#' @param start a start date to be returned in date format
#' @param end an end date to be returned in date format
#' @param unit the filename of the ALMS unit to search for
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe of cattle RFID numbers and calculated calving dates
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


calc_calvingdate <- function(RFID, start, end, unit=NULL, username=NULL, password=NULL){

  if(is.null(username) || is.null(password)){
  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)
  }

  weeklywts <- get_weeklywts(RFID = RFID, start = start, end = end, #unit = unit, there is no unit command for get weeklywts
                             fields = c("RFID", "Date", "avweight"),
                             username = username, password = password)

  dailywts <- get_dailywts(RFID = RFID, start = start, end = end, #unit = unit,
                           fields = c("RFID", "Wt", "datetime"),
                           username = username, password = password)

  # This section calculates the CALVING WEEK from the weeklywts dataframe

  calvingdata <- data.frame()

  for(i in 1:length(RFID)){

    rfid <- RFID[i]

    subdata <- weeklywts%>%
      filter(weeklywts$RFID == rfid)%>%
      mutate(timediff = as.numeric(Date-lag(Date), units="weeks"),
             wtdiff = round(avweight - lag(avweight),2))

    if(nrow(subdata) > 1){ # Will select cows that have 2 or more weekly weights

      n <- which(subdata$wtdiff == min(subdata$wtdiff, na.rm=TRUE))

      MinWt <- subdata$wtdiff[n]
      TimeDiff <- subdata$timediff[n]
      ALMSCalfWeek <- subdata$Date[n]
      ALMSCalfWeekWt <- subdata$avweight[n]
      PreALMSCalfWeek <- subdata$Date[n-1]
      PreALMSCalfWeekWt <- subdata$avweight[n-1]
      PrePreALMSCalfWeek <- subdata$Date[n-1]
      FirstWeeklyWt <- subdata$Date[1]
      LastWeeklyWt <- subdata$Date[length(subdata$Date)]

      row <- data.frame(RFID = rfid, MinWt, TimeDiff, ALMSCalfWeek, ALMSCalfWeekWt, PreALMSCalfWeek, PreALMSCalfWeekWt, PrePreALMSCalfWeek,FirstWeeklyWt, LastWeeklyWt, stringsAsFactors = F)

      row <- row[1,] #  Occasionally a cow will have two MinWt's as the same weight. Maybe we go with the first week first
      # and can return to the second week if a calving date is not found

     # calvingweek <- rbind(calvingweek, row[1,])

    }
  #}

  # This specifies that the calving week weight difference has to be a negative value
  # Perhaps we can put some limits around the required weight loss here?
  # Need to do this seperatly so we can tell those cows that had insufficient weight data to those cows that demonstrated no weight loss

  row <- row%>%
                 mutate(ALMSCalfWeek = ifelse(MinWt >= 0, NA, as.character(ALMSCalfWeek)),
                        ALMSCalfWeek = ifelse(is.na(ALMSCalfWeek), as.Date(ALMSCalfWeek, tz = "Australia/Brisbane"), as.character(ALMSCalfWeek)),
                        TimeDiff = ifelse(MinWt >= 0, NA, TimeDiff),
                        ALMSCalfWeekWt = ifelse(MinWt >= 0, NA, ALMSCalfWeekWt),
                        PreALMSCalfWeek = ifelse(MinWt >= 0, "NA", as.character(PreALMSCalfWeek)),
                        PreALMSCalfWeek = ifelse(PreALMSCalfWeek != "NA", as.Date(PreALMSCalfWeek, tz = "Australia/Brisbane"), as.character(PreALMSCalfWeek)),
                        PreALMSCalfWeekWt = ifelse(MinWt >= 0, NA, PreALMSCalfWeekWt),
                        PrePreALMSCalfWeek = ifelse(MinWt >= 0, "NA", as.character(PrePreALMSCalfWeek)),
                        PrePreALMSCalfWeek = ifelse(PrePreALMSCalfWeek != "NA", as.Date(PrePreALMSCalfWeek, tz = "Australia/Brisbane"), as.character(PrePreALMSCalfWeek)))


  # This section calculates the CALVING DATE from the dailywts dataframe
  # The code firstly filters the daily weight data for the period around the time of calving, from the identified calving week (Sunday),
  # and goes back 6 days before the previous weekly weight.
  # Any daily weights that are greater than the previous weekly weight (the pre-calving weight) + 120 kg are then removed.
  # There is an aweful lot of filtering happening here. Needs consolidating

  # This cleanup function removes big peaks and troughs in the daily data so that erroneous weights do not affect the algorithm
  # For example, when an unusually large weight is recorded the difference between this weight and the next 'normal' weight can be detected as the calving date
  # Similarily, an unusually small weight can be detected as the calving date

  cleanup <- function(x, n){
    for(l in 2:(length(x)-1)){
      if (is.na(x[l]) | l == 2 & x[(l-1)] < x[l]-80){} else {
        if (is.na(x[l-1])){h <- tail(which(!(is.na(x[1:(l-1)]))), n=1)}else{h <- l-1}
        if (is.na(x[l+1])){t <- l + head(which(!(is.na(x[(l+1):length(x)]))), n=1)}else{t <- l+1}
        if (n > 0){if (x[l] > mean(c(x[h], x[t])) + n){x[l] <- NA}}
        if (n < 0){if (x[l] < mean(c(x[h], x[t])) + n){x[l] <- NA}}}
    }
    return(x)}

  # Transferring the daily weight data to another dataset

 # calvingdate <- data.frame()

  #for(i in 1:length(RFID)){

  #  rfid <- RFID[i]

  #  l <- which(calvingweek$RFID == rfid)

  #  if(length(l) == 0){}else{ #This filters out cows that did not have a calving week calculated

      if(is.na(row$ALMSCalfWeek)){}else{ #This filters out cows that did not demonstrate a weight loss between weekly weights

        calfweek <- row$ALMSCalfWeek
        calfweekwt <- row$ALMSCalfWeekWt
        precalfweek <- row$PreALMSCalfWeek
        precalfweekwt <- row$PreALMSCalfWeekWt
        preprecalfweek <- row$PrePreALMSCalfWeek

        datasub <- dailywts%>%
          filter(dailywts$RFID == rfid,
                 between(as.Date(Date, tz = "Australia/Brisbane"), preprecalfweek+1, calfweek),
                 Weight < precalfweekwt + 120)

        datasub$Weight <- cleanup(datasub$Weight, n = -120)
        datasub$Weight <- cleanup(datasub$Weight, n = 60)

        datasub <- datasub %>%
          filter(Weight !="NA")%>%
          mutate(wtdiff = round(Weight - lag(Weight),2)) %>%
          filter(between(wtdiff, -200, -30),
                 between(Weight, calfweekwt-80, calfweekwt+40))

        if(nrow(datasub) != 0){

          n <- which(datasub$wtdiff == min(datasub$wtdiff, na.rm=TRUE))

          MinWt <- datasub$wtdiff[n]
          ALMSCalfDate <- as.Date(datasub$Date[n], tz = "Australia/Brisbane")

          newrow <- data.frame(RFID = rfid, MinWt, ALMSCalfDate, stringsAsFactors = F)

          calvingdate <- rbind(calvingdate, newrow)

        }}}

  return(calvingdate)

}
