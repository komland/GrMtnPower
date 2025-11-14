## programmer Kristian Omland
## github.com/komland/GrMtnPower/
## 2019-11-15

source("getGMPdata.r")

## load previously retrieved data
(availableFiles <- list.files("data/"))
if(length(availableFiles) == 1){
    dat4 <- readRDS(paste0("data/", availableFiles))
}else{
    dat4 <- readRDS(paste0("data/dat4Gotten",
                           max(as.numeric(substr(availableFiles, 11, 18))),
                           ".RDS"))
}

## retrieve data since latest
(latestDate <- as.IDate(dat4[.N, dateTime]))
(currentDate <- as.IDate(Sys.time()) - 1)
# currentDate <- as.IDate("2025-08-19")
if(difftime(currentDate, latestDate, units = "days") > 30){
    # stop("\nBreak up request into smaller chunks")
    currentDate <- latestDate + 30
    print(cat("using:", as.character(currentDate), "\n"))
}

dat3 <- buildAndGet(intrvl = "hourly",
                    strDt = latestDate,
                    endDt = currentDate)
                    
setdiff(names(dat3), names(dat4))
# totalEnergyUsed added summer 2024, same as my `totalConsumed`
dat3 <- dat3[, !"totalEnergyUsed", with=FALSE]

dat3[,c("dateTime", "dateForm") := .(
    as.POSIXct(sub("Z", "", sub("T", " ", date)), tz = "America/New_York"),
    as.IDate(date))]

## check for unpopulated rows
## as long as they are ~today~, lop them off
if(dat3[is.na(consumed), .N] > 0){
    if(dat3[is.na(consumed), min(dateForm)] == Sys.Date()){
        dat3 <- dat3[!is.na(consumed)]
    }else{
        stop("\nCheck on missing data")
    }
}

## verify agreement between consumed and consumedTotal
if(all(dat3[,consumed] == dat3[,consumedTotal])){
    names(dat3)[names(dat3) == "consumed"] <- "consumedFromGrid"
}else{
    stop("\nCheck \'consumed\' and \'consumedTotal\'")
}

## compute two other quantities
dat3[,consumedGeneration := generation - returnedGeneration]
dat3[,totalConsumed := consumedFromGrid + consumedGeneration]

## check for agreement of columns - sparseness of temperature known
if(length(setdiff(names(dat4), names(dat3))) == 0){
    dat5 <- rbind(dat4[!date %chin% dat3[,date]], # use fresher rows from dat3
                  dat3,
                  fill = TRUE)
}else{
    if(setdiff(names(dat4), names(dat3)) == "temperature"){
        dat5 <- rbind(dat4[!date %chin% dat3[,date]], # use fresher rows from dat3
                      dat3,
                      fill = TRUE)
    }
    stop("\nCheck missing columns")
}

## verify losing no information
foo <- dat4
setkey(foo, date)
bar <- dat5[date %chin% dat4[,date]]
setkey(bar, date)

# all.equal(foo, bar)
all.equal(foo, bar, check.attributes = FALSE)

if(all.equal(foo, bar)){
  saveRDS(dat5, file = paste0("data/dat4Gotten", gsub("-", "", Sys.Date()), ".RDS"))
}else{
    stop("\nCheck for lost data")
}
