bikedata <- read.csv("data/hour.csv")
bikedata2011 <- subset(bikedata, yr==0)

## There is only one instance of heavy precipitation because the
## network is shutdown during bad weather. In fact the one instance of
## "heavy" (4) weather occurrs just before a shutdown on 26 January. We
## reclassify it as "light" (3).
bikedata2011$weathersit[bikedata2011$weathersit == 4] <- 3

## Humidity is recorded as zero for every our of 10 March. This is implausible
## as humidity rises to 0.93 at the end of the previous day and starts at 1.00 the
## following day. We recode it as 1.00 (maximum humidity) 
bikedata2011[bikedata2011$dteday=="2011-03-10", "hum"] <- 1

## Create an indicator variable for the weekend
bikedata2011$weekend <- bikedata2011$weekday %in% c(0, 6)

## Gaps in the record occur when there are no users at all, i.e. when cnt==0.
## This can occur in the middle of the night and we should restore
## these records because they are informative. There are four long gaps in the
## record when the network was shutdown. These ones we identify as having a gap
## of 6 hours or longer, bearing in mind that the gap may appear negative if
## it includes midnight. There are no gaps longer than 23 hours.
gap <- diff(bikedata2011$hr) - 1
gap <- gap %% 24

datetime <- expand.grid(hr=0:23, "dteday"=as.Date("2011-01-01") + 0:364)
datetime$shutdown <- FALSE

shutdown <- subset(bikedata2011, gap >= 6)
shutdown$gap <- gap[gap >= 6]
for (i in 1:nrow(shutdown)) {
  index <- which(datetime$dteday==shutdown[i,"dteday"] & datetime$hr==shutdown[i,"hr"])
  datetime[index + 1:shutdown[i, "gap"], "shutdown"] <- TRUE
}
## Also classify the network as "shutdown" in the EST-EDT time jump
## from 2am to 3am on 13 January. The reverse time jump on 11 November
## does not seem to be a problem
datetime[datetime$dteday=="2011-03-13" & datetime$hr==2, "shutdown"] <- TRUE

## Now merge to create a data frame with 365 x 24 = 8760 rows
bikedata2011 <- merge(datetime[,c(2,1,3)], bikedata2011, all=TRUE)

## Fill in the blanks. When there is no rental we carry all predictor variables forward
## from the last known value. Set the response variables - casual, registered, cnt - to
## zero unless the network is shutdown, in which case they remain missing.
norental <- which(is.na(bikedata2011$cnt))
missing.vars <- c("season", "mnth", "holiday", "weekday", "weathersit", "temp", "atemp",
                  "hum", "windspeed")
for (i in norental) {
    ## Carry last value forward, except for date and time
    bikedata2011[i,missing.vars] <- bikedata2011[i-1, missing.vars]
    if (bikedata2011[i, "shutdown"] == FALSE) {
        bikedata2011[i, c("casual","registered","cnt")] <- 0
    }
}
