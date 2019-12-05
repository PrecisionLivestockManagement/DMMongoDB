#' Check cattle access to ALMS using RFID number
#'
#' This function pulls in daily weights for individual or groups of cattle for specified periods. It searches based on a list of RFID values. It is recommended that you use the propsearch function to find a list of cattle RFID numbers for a particular property. Unlike propsearch this function provides a quick tool to check most recent activity and identify when cattle last visited an ALMS. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name RFIDcattlecheck
#' @param RFID this is a list of cattle RFID numbers
#' @param days the number of days to search over, if no days entered then it only searches for cattle that have crossed the ALMS today
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a dataframe that provides a list including the RFID number, datetime and weights of cattle
#' @author Dave Swain \email{dave.swain@@datamuster.net.au} and Lauren O'Connor \email{lauren.oconnor@@datamuster.net.au}
#' @import dplyr
#' @export


RFIDcattlecheck <- function(RFID, days=NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

if(is.null(days)){
  lump <- Sys.Date()
  }  else {
  lump <- Sys.Date() - days
  }

  checkanimals <- dailywtsNEW(RFID, username=username, password=password)

checkanimals <- bind_rows(checkanimals$DailyWeights, .id = "RFID")%>%
  filter(as.Date(Date) >= lump)%>%
  arrange(Date)%>%
  mutate(TimeSinceLastVisit = round(Sys.time()-Date,1))

checkanimals

}


