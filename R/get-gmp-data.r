## programmer Kristian Omland
## github.com/komland/
## 2019-10-28

library(base64enc)
library(httr)
library(jsonlite)
library(data.table)

##################################################################################
## ##         Credentials are read from environment variables               ## ##
## ## Set these in .Renviron (project root or home directory):              ## ##
## GMP_ACCOUNT=your-account-number                                         ## ##
## GMP_KEY_ID=your-api-key-id                                              ## ##
## GMP_KEY_SECRET=your-api-secret                                          ## ##
##################################################################################

## Read credentials from environment
GMPaccount <- Sys.getenv("GMP_ACCOUNT")
apiKeyId <- Sys.getenv("GMP_KEY_ID")
apiKeySecret <- Sys.getenv("GMP_KEY_SECRET")

## Verify credentials are present
if (GMPaccount == "" || apiKeyId == "" || apiKeySecret == "") {
  stop("Missing GMP credentials in environment variables.\n",
       "Set GMP_ACCOUNT, GMP_KEY_ID, and GMP_KEY_SECRET in .Renviron")
}
## assemble/encode the API key ID and secret as header to be supplied with request
idSecret <- paste(apiKeyId, ":", apiKeySecret, sep = "")
b64Key <- base64encode(charToRaw(idSecret))
reqHead <- paste("Basic ", b64Key, sep = "")
## with those objects having been assembled in the form needed, rm for privacy
## nb GMPaccount not removed - used directly and included in the output anyway
rm(idSecret, b64Key)

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
