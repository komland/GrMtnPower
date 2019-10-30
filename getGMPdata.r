library(base64enc)
library(httr)

###################################################################################
## ## begin user-supplied variables ## ##
## account credentials stored in a text file, which is kept private
## to see what my file looks like, you could run ...
## cat(paste("GMPaccount: ",
##           paste(sample(x = 0:9, size = 10, replace = TRUE),
##                 collapse = ""),
##           "\n",
##           "apiKeyId: ",
##           paste(sample(x = c(0:9, letters, LETTERS), size = 25, replace = TRUE),
##                 collapse = ""),
##           "\n",
##           "apiKeySecret: ",
##           paste(sample(x = c(0:9, letters, LETTERS), size = 50, replace = TRUE),
##                 collapse = ""),
##           "\n",
##           sep = ""))
credentials <- readLines("../credentials.txt")
GMPaccount <- strsplit(credentials[1], " ")[[1]][2]
apiKeyId <- strsplit(credentials[2], " ")[[1]][2]
apiKeySecret <- strsplit(credentials[3], " ")[[1]][2]

## other (~ self-explanatory) user-supplied variables
requestInterval <- "daily" # options: hourly, daily, or monthly
startDate <- "2018-10-30"
## not "too long;" 31 days x hourly and full year of daily were OK
endDate <- "2019-10-29"
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

## automatic extraction of the variable names
if(exists("dat4Names")) rm(dat4Names)
for(i in 1:length(GMPcontent$intervals[[1]]$values)){
    namesInt <- names(GMPcontent$intervals[[1]]$values[[i]])
    if(!exists("dat4Names")){
        dat4Names <- namesInt
    }else{
        dat4Names <- union(dat4Names, namesInt)
    }
}

if(exists("dat4")) rm(dat4)
for(i in 1:length(GMPcontent$intervals[[1]]$values)){
    dfInt <- data.frame(GMPcontent$intervals[[1]]$values[[i]],
                        stringsAsFactors = FALSE)
    ## supply NAs if any of the desired variables is not populated
    dfInt[,setdiff(dat4Names, names(dfInt))] <- NA
    ## and ensure they are in the same order
    dfInt <- dfInt[,dat4Names]
    if(!exists("dat4")){
        dat4 <- dfInt
    }else{
        dat4 <- rbind(dat4, dfInt)
    }
}

## ## some basic data preparation ## ##
## while leaving original date variable untouched, populate
## - formatted date
## - date-time (if hourly)
if(requestInterval == "hourly"){
    dat4$dateTime <- as.POSIXct(sub("Z", "", sub("T", " ", dat4$date)),
                                tz = "America/New_York")
    dat4$dateForm <- as.Date(sub("T00:00:00Z", "", dat4$date))
}else{
    dat4$dateForm <- as.Date(sub("T00:00:00Z", "", dat4$date))
}

dat4[dat4$consumed != dat4$consumedTotal,] ## always empty, so far

## clarifying and fully populating consumption
names(dat4)[names(dat4) == "consumed"] <- "consumedFromGrid"
dat4$consumedGeneration <- dat4$generation - dat4$returnedGeneration
dat4$totalConsumed <- dat4$consumedFromGrid + dat4$consumedGeneration
