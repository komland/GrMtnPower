library(lattice)

dat5[, range(dateTime)]

dat5[, solarYear := fifelse(month(dateForm) %in% c(11, 12),
                            year(dateForm) + 1,
                            year(dateForm))]

dat5[, bev := dateForm %between% as.IDate(c("2016-06-01", "2019-05-31"))]

datDaily <- dat5[, .(generated = sum(generation, na.rm = TRUE),
                     consumed = sum(consumedTotal, na.rm = TRUE),
                     solarYear = max(solarYear),
                     bev = max(bev)), dateForm]

xyplot(generated + consumed ~ dateForm, datDaily, pch = 18, type = "b")

# last 12 months
ya <- as.IDate(sub(as.character(year(Sys.Date())),
                   as.character(year(Sys.Date()) - 1),
                   as.character(Sys.Date())))
xyplot(generated + consumed ~ dateForm,
       datDaily[dateForm >= ya],
       pch = 18, type = "b")

# every month m
m <- 8
xyplot(generated + consumed ~ dateForm | as.factor(year(dateForm)),
       datDaily[month(dateForm) == m],
       pch = 18, type = "b",
       scales = list(x = list(relation = "free", rot = 90)),
       as.table = TRUE, layout = c(6, 1), strip = FALSE, strip.left = TRUE)


# by quarter
datWeekly <- dat5[, .(nObs = .N,
                      generated = sum(generation, na.rm = TRUE),
                      consumed = sum(consumedTotal, na.rm = TRUE),
                      solarYear = max(solarYear),
                      bev = max(bev)),
                  .(year(dateForm), isoweek(dateForm))]
datWeekly <- datWeekly[nObs >= 167]
Quarters <- matrix(c(1, 13, 14, 26, 27, 39, 40, 52), byrow = TRUE, nrow = 4)
q <- 2
xyplot(generated + consumed ~ isoweek | as.factor(year),
       datWeekly[isoweek %between% c(14, 26)],
       pch = 18, type = "b",
       as.table = TRUE, layout = c(6, 1), strip = FALSE, strip.left = TRUE)


# since bev, what proportion of our generation have we consumed?
stDateSeq <- seq(dat5[bev == 1, max(dateForm)] + 1,
                 as.IDate("2020-09-13"),
                 "day")
for(i in 1:length(stDateSeq)) {
        focalD <- stDateSeq[i]
        y <- dat5[dateForm >= focalD & dateForm < focalD + 365,
                  .(date = focalD, p = sum(consumedTotal)/sum(generation))]
        if (i == 1) {
                yDT <- y
        }
        else{
                yDT <- rbind(yDT, y)
        }
}
yDT[,plot(date, p)]
yDT[,summary(p)]
abline(h = 0.65)
