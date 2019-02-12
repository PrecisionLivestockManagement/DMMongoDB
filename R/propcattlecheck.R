#' Check cattle access to ALMS accross whole property
#'
#' This function pulls in daily weights for cattle for specified periods. It searches based on all cattle across a named property. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name propcattlecheck
#' @param property this is a list or single property name as a character entry
#' @param days the number of days to search over, if no days entered then it only searches for cattle that have crossed the ALMS today
#' @return a dataframe that provides a list including the RFID number, datetime and weights of cattle
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import dplyr
#' @export


propcattlecheck <- function(property, days=NULL){

  bung <- propsearch(property)

  if(is.null(days)){
    lump <- Sys.Date()
  }
  else {
    lump <- Sys.Date() - days}

  checkanimals <- dailywts(bung$RFID)
  checkanimals <- bind_rows(checkanimals$DailyWeights, .id = "RFID")%>%
    filter(as.Date(as.POSIXct(Date, tz = "Australia/Brisbane"),tz = "Australia/Brisbane") >= lump)%>%
    arrange(desc(Date))%>%
  mutate(TimeSinceLastVisit = Sys.time()-Date)

  checkanimals

}
