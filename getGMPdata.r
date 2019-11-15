## programmer Kristian Omland
## github.com/komland/
## 2019-10-28

library(base64enc)
library(httr)

##################################################################################
## ##         the only user-supplied variable is the credential file         ## ##
credentials <- readLines("../credentials.txt")
## ## to see what my credentials file looks like, you could uncomment and run ...
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
##################################################################################

## extracting the three variables stored in the text
GMPaccount <- strsplit(credentials[1], " ")[[1]][2]
apiKeyId <- strsplit(credentials[2], " ")[[1]][2]
apiKeySecret <- strsplit(credentials[3], " ")[[1]][2]
## assemble/encode the API key ID and secret as header to be supplied with request
idSecret <- paste(apiKeyId, ":", apiKeySecret, sep = "")
b64Key <- base64encode(charToRaw(idSecret))
reqHead <- paste("Basic ", b64Key, sep = "")
## with those objects having been assembled in the form needed, rm for privacy
## nb GMPaccount not removed - used directly and included in the output anyway
rm(credentials, apiKeyId, apiKeySecret, idSecret, b64Key)

## base API
GMPapiURL <- "https://api.greenmountainpower.com/api/v2/usage/"

## function to build request and get data
buildAndGet <- function(intrvl, strDt, endDt){
    ## fully assembled URL
    GMPapi <- paste(GMPapiURL, GMPaccount, "/", intrvl,
                    "?startDate=", strDt,
                    "T00:00:00Z&endDate=", endDt, "T23:59:59Z&temp=f",
                    sep = "")

    ## data request
    GMPrequest <- httr::GET(url = GMPapi, add_headers(Authorization = reqHead))
    if(GMPrequest$status_code == 200){
        output <- list()
        ## include time object ... for curiosity
        output[[1]] <- GMPrequest$time

        ## parse content; parsing directly to an R object seems to work fine
        GMPcontent <- content(GMPrequest, as = "parsed")
        ## ## review ~ metadata
        ## names(GMPcontent)
        ## GMPcontent$accountNumber
        ## names(GMPcontent$intervals[[1]])
        ## ## ~ data are in values processed below, but can look at these:
        ## GMPcontent$intervals[[1]][1:5]

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
        if(intrvl == "hourly"){
            dat4$dateTime <- as.POSIXct(sub("Z", "", sub("T", " ", dat4$date)),
                                        tz = "America/New_York")
            dat4$dateForm <- as.Date(sub("T00:00:00Z", "", dat4$date))
        }else{
            dat4$dateForm <- as.Date(sub("T00:00:00Z", "", dat4$date))
        }

        ## dat4[!is.na(dat4$consumed) &
        ##      !is.na(dat4$consumedTotal) &
        ##      dat4$consumed != dat4$consumedTotal,] ## always empty, so far
        ## instead, here testing whether they are equal
        ## (and throwing an error if they are not)
        if(any(!is.na(dat4$consumed) &
               !is.na(dat4$consumedTotal) &
               dat4$consumed != dat4$consumedTotal)){
            stop("check \'consumed\' and \'consumedTotal\'")
        }

        ## clarifying and fully populating consumption
        names(dat4)[names(dat4) == "consumed"] <- "consumedFromGrid"
        dat4$consumedGeneration <- dat4$generation - dat4$returnedGeneration
        dat4$totalConsumed <- dat4$consumedFromGrid + dat4$consumedGeneration

        ## include time object ... for curiosity
        output[[2]] <- dat4

        ## return the list with time and data
        return(output)
    }else{
        stop(paste("Status Code", GMPrequest$status_code))
    }
}
