library(data.table)
library(mgcv)
library(lubridate)
library(solarPos)
library(insol)
library(lattice)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73",
               "#F0E442",# "#0072B2",
               "#D55E00", "#CC79A7")
trellis.par.set("superpose.line" = list(col = cbPalette),
                "superpose.symbol" = list(pch = 18, col = cbPalette))

## load data
availableFiles <- list.files("data/")
if(length(availableFiles) == 1){
  dat4 <- readRDS(paste0("data/", availableFiles))
}else{
  dat4 <- readRDS(paste0("data/dat4Gotten",
                         max(as.numeric(substr(availableFiles, 11, 18))),
                         ".RDS"))
}

# model of generation based on daily and seasonal components
dat4[, timeDay := as.numeric(as.ITime(dateTime))/(24*60^2)]
dat4[, dayYear := as.POSIXlt(date)$yday/366]
fmGen <- gam(formula = generation ~ s(dayYear, timeDay), data = dat4)
plot(fmGen)

# but I really want a hierarchical model where
# potential is smooth over timeDay within dayYear (basically like fmGen)
# but loss is always positive, occasionally 0, gamma
foo <- dat4[!is.na(generation),.(dateTime, generation)]
foo[, JD := JD(dateTime)]
sunPos <- sunpos(sunvector(foo[,JD], 44.468674, -72.979348, -5))
foo[, c("azimuth", "zenith") := .(sunPos[,1], sunPos[,2])]
foo[generation > 0.5, range(azimuth)]

fmGen <- gam(generation ~ s(azimuth, zenith),
             data = foo[zenith <= 90 & azimuth %between% c(135, 315)])
plot(fmGen, scheme = 2)

foo[,plot(azimuth, generation)]

foo[plot(lubridate::hour(dateTime), zenith)]
foo[plot(lubridate::yday(dateTime), zenith)]
foo[plot(1:.N, zenith)]


foo[, dst := dst(dateTime)]
foo[, UTCtime := as.POSIXct(ifelse(dst, dateTime + 4/24, dateTime + 4/24), tz = "UTC", origin = "1970-01-01")]
foo[, c("jYear", "jMonth", "jDay", "jHour") :=
      .(lubridate::year(UTCtime),
        lubridate::month(UTCtime),
        lubridate::mday(UTCtime),
        lubridate::hour(UTCtime))]
foo[, julianDay := julianDay(year = jYear,
                             month = jMonth,
                             day = jDay,
                             hour = jHour)]
foo[, JD := JD(UTCtime)]
foo[, JDnaive := JD(dateTime)]
foo


foo[,{
  sp <- solarPosition(jd = foo[,julianDay], lon = -72.979348, lat = 44.468674)
  list(zenith = sp[,1],
       azimuth = sp[,2])
}]

# [, c("year", "month", "day", "hour") :=
#       .(fooDate$year + 1900,
#         fooDate$mon + 1,
#         fooDate$mday,fooDate$hour)]


## most recent 30 days
lastMonth <- dat4[dateTime %between% c(dat4[,max(dateTime)] - 30 * 24 * 60^2,
                                       dat4[,max(dateTime)])]

## median load by hour
# dat4[,median(consumedFromGrid, na.rm = TRUE)]
# lastMonth[,median(consumedFromGrid, na.rm = TRUE)]
# plot(dat4[,median(consumedFromGrid, na.rm = TRUE), year(dateForm) + (month(dateForm) - 0.5)/12],
#      type = "l", xlab = "", ylab = "Median Load")

## compute daily totals
dat5 <- lastMonth[, .(consumed = sum(consumedFromGrid, na.rm = TRUE),
                      returned = sum(returnedGeneration, na.rm = TRUE),
                      generated = sum(generation, na.rm = TRUE)),
                   dateForm]
# dat5[,median(consumed/24)]
xyplot(consumed + returned + generated ~ dateForm, dat5,
       type = "b", xlab = "", ylab = "W",
       auto.key = list(space = "right"),
       scales = list(x = list(at = dat5[wday(dateForm)== 1, dateForm])),
       panel = function(x,y,...){
         panel.abline(v = dat5[,dateForm], col = gray(0.9))
         panel.abline(v = dat5[wday(dateForm)== 1, dateForm], col = gray(0.6))
         panel.abline(h = 0:50, col = gray(0.9))
         panel.abline(h = seq(0, 50, 10), col = gray(0.6))
         panel.xyplot(x,y,...)
       })
# ## median load by day
# xyplot(consumed/24 ~ dateForm, dat5, type = "l",
#        xlab = "", ylab = "Mean Load (kW)")
# ## generation
# xyplot(generated ~ dateForm, dat5, type = "l",
#        xlab = "", ylab = "Generation (kWh)")
