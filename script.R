rm(list = ls())

# Function
options(stringsAsFactors = FALSE) 
multipleCombine <- function(input, ply = llply){
  require(plyr)
  require(dplyr)
  ply(input, function(x){
    t <- read.table(x, header=TRUE, sep=",",stringsAsFactors = FALSE)
    t1 <- rbind(t) 
    return(t1)
  }
  )
}

# Libraries
library(RCurl)
library(XML)
library(Zillow)
library(plyr)
library(dplyr)
library(foreign)


# Paths
setwd("~/GitHub/boulder-open-data")
pathToData <- paste(getwd(),"zillowData",sep="/")

# To get Zillow data you need a zillow ID
zillowId <- read.table(paste(getwd(),"ZWSID.txt",sep="/"), encoding="UTF8",
                       row.names=NULL, quote="", comment.char="", stringsAsFactors=FALSE)[1,]

# Boulder addresses
boulderList <- read.dbf(paste(getwd(),"cityData","Boulder_addresses.dbf",sep="/"))

# Do I already have some of this data?
addressIDs <- sort(unique(boulderList$ADDRESSID))

# Locations that I already have data from
fileList <- list.files(pathToData, full.names=TRUE)
boulder.dbf <- multipleCombine(fileList, ply = ldply)
boulder.dbf %>% filter(is.na(price)==FALSE)-> boulder.dbf
boulder.dbf <- unique.data.frame(boulder.dbf)

# Remove locations I don't need to check
addressIDs <- addressIDs[addressIDs%in%boulder.dbf$ADDRESSID==FALSE]
addressIDs <- sample(addressIDs,2500,replace = FALSE)
boulderList$price <- NA

t <- addressIDs[1]
for (j in addressIDs){ 
  tmp <- try(boulderList$price[boulderList$ADDRESSID==j] <- as.numeric(zestimate(boulderList$ADDRESS[j],
                                                                                 boulderList$ZIPCODE[j],zillowId)$amount[1]))
  if (class(tmp)=="try-error"){
    boulderList$price[boulderList$ADDRESSID==j] <- NA
    j <- addressIDs[match(j,addressIDs)+1]
  }
#   if(match(j, addressIDs)%%1000==0){
#     write.csv(boulderList[addressIDs[t:j],],file = paste(paste(pathToData,j,sep='/'),'csv',sep="."),
#               row.names = FALSE)
#     t <- addressIDs[match(j,addressIDs)+1]
#   }
}
all <- boulderList[is.na(boulderList$price)==FALSE,]
write.csv(all,file = paste(paste(pathToData,all$ADDRESSID[dim(all)[1]],sep='/'),'csv',sep="."),
          row.names = FALSE)