## programmer Kristian Omland
## github.com/komland/
## 2019-10-28

import requests
import base64
import json

###################################################################################
## ## begin user-supplied variables ## ##
## account credentials; these will be private
GMPaccount = ""
apiKeyId = ""
apiKeySecret = ""

## other (~ self-explanatory) user-supplied variables
requestInterval = "hourly" # can also be "daily" or "monthly"
startDate = "2016-11-05"
endDate = "2016-11-05" # not "too long;" 31 days x hourly was OK
## ## end user-supplied variables ## ##
###################################################################################

## base API
GMPapiURL = "https://api.greenmountainpower.com/api/v2/usage/"

## fully assembled URL
GMPapi = (GMPapiURL + GMPaccount + "/" + requestInterval + "?startDate=" + startDate + "T00:00:00Z&endDate=" + endDate + "T23:59:59Z&temp=f")

## assemble/encode the API key ID and secret as header to be supplied with request
idSecret = apiKeyId + ":" + apiKeySecret
b64Key = str(base64.b64encode(idSecret.encode("utf-8")), "utf-8")
reqHead = {
    "Authorization": "Basic %s" % b64Key
}

## data request
GMPcontent = requests.get(GMPapi, headers = reqHead)
print(GMPcontent.status_code)
print(GMPcontent.elapsed.total_seconds())

json_GMPcontent = GMPcontent.json()
json_GMPcontent["accountNumber"]
json_GMPcontent["intervals"] ## yikes! non-python coder here
## dump to json file
with open("..\dat4.json", "w") as outfile:
    json.dump(json_GMPcontent, outfile)
