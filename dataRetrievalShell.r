## programmer Kristian Omland
## github.com/komland/GrMtnPower/
## 2019-11-15

## a simple example of running my R script to retrieve meter data from my GMP account

## load previously retrieved data
dat4 <- readRDS("../dat4Gotten20210126.RDS")

source("getGMPdata.r")

## firstMonths <- as.IDate(paste0("2020-", c(paste0("0", 1:9), 10:12), "-01"))
firstMonths <- as.IDate(c("2021-01-01", "2021-02-01"))
lastMonths <- firstMonths[2:length(firstMonths)] - 1
firstMonths <- firstMonths[1:(length(firstMonths) - 1)]
cbind(as.character(firstMonths), as.character(lastMonths))

for(i in 1:length(firstMonths)){
    print(i)
    dat3 <- buildAndGet(intrvl = "hourly", # hourly, daily, or monthly
                        strDt = firstMonths[i],
                        endDt = lastMonths[i])

    if(all(dat3[,consumed] == dat3[,consumedTotal])){
        names(dat3)[names(dat3) == "consumed"] <- "consumedFromGrid"
    }else{
        stop("check \'consumed\' and \'consumedTotal\'")
    }

    dat3[,c("dateTime", "dateForm") := .(
              as.POSIXct(sub("Z", "", sub("T", " ", date)), tz = "America/New_York"),
              as.IDate(date))]

    dat3[,consumedGeneration := generation - returnedGeneration]
    dat3[,totalConsumed := consumedFromGrid + consumedGeneration]

    dat4 <- rbind(dat4, dat3)
}

saveRDS(dat4, file = paste0("../dat4Gotten", gsub("-", "", Sys.Date()), ".RDS"))
