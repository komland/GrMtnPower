## programmer Kristian Omland
## github.com/komland/
## 2019-11-15

## a simple example of running my R script to retrieve meter data from my GMP account

source("getGMPdata.r")

dat3 <- buildAndGet(intrvl = "hourly", # hourly, daily, or monthly
                    strDt = "2017-10-23",
                    endDt = "2017-11-05")
## limitations appears to be:
## - 1 month of hourly or
## - 1 year of daily
## appears I can get a day more than that:
## - 32 days of hourly
## - 366 days of daily for a non-leap year or 367 days for a leap year ...
##   but not 367 days for a non-leap year!

## 1 month of hourly: 31 * 24 = 744 records works
## still works with an extra day (768 records) but not 33 days (792 records)
## 1 year of daily, whether leap year (366 records) or not (365 works)
## you can get an extra day (366 for non-leap, 367 for leap) ...
## but not necessarily 367 (not 2 extra days on a non-leap year!)

## review time report
dat3[[1]]

## parsed data
head(dat4 <- dat3[[2]])
tail(dat4)
