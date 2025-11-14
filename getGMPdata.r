## programmer Kristian Omland
## github.com/komland/
## 2019-10-28

library(base64enc)
library(httr)
library(jsonlite)
library(data.table)

##################################################################################
## ##         the only user-supplied variable is the credential file         ## ##
credentials <- readLines("credentials.txt")
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
        return(setDT(fromJSON(content(GMPrequest, as = "text"))$intervals$values[[1]]))
    }else{
        stop(paste("Status Code", GMPrequest$status_code))
    }
}
