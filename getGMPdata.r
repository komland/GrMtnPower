library(base64enc)
library(httr)

###################################################################################
## ## begin user-supplied variables ## ##
## account credentials; these will be private
GMPaccount <- ""
apiKeyId  <-  ""
apiKeySecret  <-  ""

## other (~ self-explanatory) user-supplied variables
requestInterval <- "hourly" # can also be "daily" or "monthly"
startDate <- "2019-09-01"
endDate <- "2019-09-30" # not "too long;" 31 days x hourly was OK
## ## end user-supplied variables ## ##
###################################################################################

## base API
GMPapiURL <- "https://api.greenmountainpower.com/api/v2/usage/"

## fully assembled URL
GMPapi <- paste(GMPapiURL, GMPaccount, "/", requestInterval,
                "?startDate=", startDate,
                "T00:00:00Z&endDate=", endDate, "T23:59:59Z&temp=f",
                sep = "")

## assemble/encode the API key ID and secret as header to be supplied with request
idSecret <- paste(apiKeyId, ":", apiKeySecret, sep = "")
b64Key <- base64encode(charToRaw(idSecret))
reqHead <- paste("Basic ", b64Key, sep = "")

## data request
GMPrequest <- httr::GET(url = GMPapi, add_headers(Authorization = reqHead))
GMPrequest$status_code
GMPrequest$time

## parse content; parsing directly to an R object seems to work fine
GMPcontent <- content(GMPrequest, as = "parsed")
names(GMPcontent)
GMPcontent$accountNumber
names(GMPcontent$intervals[[1]])
GMPcontent$intervals[[1]][1:5]

GMPcontent$intervals[[1]]$values[[1]]
(dat4Names <- apply(sapply(GMPcontent$intervals[[1]]$values, names), 1, unique))

if(exists("dat4")) rm(dat4)
for(i in 1:length(GMPcontent$intervals[[1]]$values)){
    dfInt <- data.frame(GMPcontent$intervals[[1]]$values[[i]])
    ## may need to supply NAs for missing variables here at some point
    dfInt <- dfInt[,dat4Names]
    if(!exists("dat4")){
        dat4 <- dfInt
    }else{
        dat4 <- rbind(dat4, dfInt)
    }
}

## some basic data preparation
dat4$dateTime  <- as.POSIXct(sub("Z", "", sub("T", " ", dat4$date)),
                              tz = "America/New_York")
dat4[dat4$consumed != dat4$consumedTotal,] ## always empty, so far
names(dat4)[names(dat4) == "consumed"] <- "consumedFromGrid"
dat4$consumedGeneration <- dat4$generation - dat4$returnedGeneration
dat4$totalConsumed <- dat4$consumedFromGrid + dat4$consumedGeneration
