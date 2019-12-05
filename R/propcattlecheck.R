#' Check cattle access to ALMS accross whole property
#'
#' This function pulls in daily weights for cattle for specified periods. It searches based on all cattle across a named property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name propcattlecheck
#' @param property this is a list or single property name as a character entry
#' @param days the number of days to search over, if no days entered then it only searches for cattle that have crossed the ALMS today
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe that provides a list including the RFID number, datetime and weights of cattle
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import dplyr
#' @export


propcattlecheck <- function(property, days=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    bung <- propsearch(property, username=username, password=password)


  if(is.null(days)){
    lump <- Sys.Date()
  }else {
    lump <- Sys.Date() - days}

  checkanimals <- dailywtsNEW(bung$RFID)
  checkanimals <- bind_rows(checkanimals$DailyWeights, .id = "RFID")%>%
    filter(as.Date(Date) >= lump)%>%
    arrange(Date)%>%
    mutate(TimeSinceLastVisit = round(Sys.time()-Date,1))

  checkanimals

}
