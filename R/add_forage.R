#' Add forage sample information to DataMuster MongoDB database.
#'
#' This function adds forage sample information to the DataMuster MongoDB database. If you need assistance please email \email{info@@datamuster.net.au} to seek help or suggest improvements.
#' @name add_forage
#' @param property the name of the property
#' @param paddock the name of the paddock
#' @param date the date of the forage assessment
#' @param method the assessment method, either 'pasture cut' or 'visual assessment'
#' @param samplesize the sample size of the estimation, either 'quadrant' or 'hectares'
#' @param wetwt the weight of the pasture as is, prior to drying (kg)
#' @param drywt the weight of the pasture after drying (kg)
#' @param photo whether or not a photograph of the assessment site was taken, TRUE or FALSE
#' @param lat the latitude coordinate of the assessment site, as a decimal
#' @param long the longitute coordinate or the assessment site, as a decimal
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
#' @return a message that indicates that the calving information has been successfully updated
#' @author Dave Swain \email{d.swain@@cqu.edu.au}, Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}, and Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import keyring
#' @import dplyr
#' @export


add_forage <- function(property, paddock, date, method, samplesize, wetwt, drywt, photo, lat = NULL, long = NULL, username=NULL, password=NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)
  }

    pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
    forage <- mongo(collection = "Forage", db = "DataMuster", url = pass, verbose = T)

    checkpads <- get_paddocks(property = "Belmont", username = username, password = password)

    notindb <- paddock[!(paddock %in% checkpads$paddname)]
    if(length(notindb) !=0){stop(print(paste0("The following paddock cannot be found in the database. Please check the paddock name and try again: ", notindb)))}

    if (length(property) == 1){property <- rep(property, length = length(drywt))}
    if (length(paddock) == 1){paddock <- rep(paddock, length = length(drywt))}
    if (length(date) == 1){date <- rep(date, length = length(drywt))}
    if (length(method) == 1){method <- rep(method, length = length(drywt))}
    if (length(samplesize) == 1){samplesize <- rep(samplesize, length = length(drywt))}
    if (length(photo) == 1){photo <- rep(photo, length = length(drywt))}

    date <- as.character(date)
    date <- as.POSIXct(date, format="%Y-%m-%d", tz = "GMT")
    date <- date - as.difftime(10, unit="hours")

    #  Update Forage collection --------------------------

    template <- forage$find(query = '{"property":"xxxxxx"}', fields = '{"_id":false}')

    for(i in 1:length(drywt)){

    template1 <- template

    template1$property <- property[i]
    template1$paddock <- as.character(paddock[i])
    template1$date <- as.POSIXct(date[i])
    template1$method <- method[i]
    template1$sample_size <- samplesize[i]
    template1$wetwt <- wetwt[i]
    template1$drywt <- drywt[i]
    template1$photo <- photo[i]
    template1$DM_perc <- round((drywt[i]/wetwt[i])*100,0)
    template1$DM_ha <- ifelse(samplesize[i] == "quadrant", round(drywt[i]*40,0), drywt[i])
    if(is.null(long)){template1$coordinates[[1]] <- c(0, 0)}else{template1$coordinates[[1]] <- c(long[i], lat[i])}

    rownames(template1)<-c()

    forage$insert(template1)
    }

}





