# Setting up an option to search all RFIDs based on history of tags if an animal has lost a tag. Currently we only have one tag with a history so will put
# this on hold until we have some more lost tags. One of the issues is the challenge of merging the S3 lists of RFIDs that have been assigned to an animal
# so we only report on the current list. The bottom section was aiming to use dplyr to do some of this but it needs a bit more thought.

library(keyring)
library(mongolite)
library(dplyr)



username = keyring::key_list("DMMongoDB")[1,2]
password =  keyring::key_get("DMMongoDB", username)

pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)

cattle <- mongo(collection = "Cattle", db = "DataMuster", url = pass, verbose = T)

RFID <- paste(unlist(bung$RFID), collapse = '", "' )
filterstation <- sprintf('{"RFID":{"$in":["%s"]}}', RFID)
#jan2 <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "wkwthist.date":true, "wkwthist.avweight":true, "_id":false}')


RFIDhist <- cattle$find(query = filterstation, fields='{"RFID":true, "stationname":true, "RFIDhist.date":true, "RFIDhist.ID":true, "_id":false}')

HistRFID <- list()

for(i in 1:length(RFIDhist$RFID)){
  
  ID <- setNames(data.frame(matrix(ncol = 2, nrow = length(RFIDhist$RFIDhist$ID[[i]]))), c("Date", "ID"))
  ID$Date <- RFIDhist$RFIDhist$date[[i]]
  ID$ID <- RFIDhist$RFIDhist$ID[[i]]
  
  #This is the section where we can apply further filters based on breed, class, etc.
  
  if(nrow(ID)<1){RFIDhist$RFID[[i]] <- "xxxx"}
  else{HistRFID[[RFIDhist$RFID[i]]] <- as.data.frame(ID)}
}

RFID <- RFIDhist[which(RFIDhist$RFID!="xxxx"),]
HistRFID <- list(RFID=RFID$RFID, Property=RFID$stationname, RFIDChanges=HistRFID)

Prep <- bind_rows(HistRFID$RFIDChanges, .id = "RFID")

Prep2 <-  Prep %>% group_by(RFID) %>% 
  summarize(IDlist = paste(sort(unique(ID)),collapse=", "))
