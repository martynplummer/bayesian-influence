source("read-data.R")

jags.data <- bikedata2011[, c("holiday", "season", "mnth", "hr", "shutdown",
                              "weekday", "hum", "weathersit", "windspeed",
                              "casual")]

jags.data$holiday <- as.numeric(jags.data$holiday)
jags.data$temp2 <- poly(bikedata2011$atemp, 2)
jags.data$shutdown <- as.numeric(jags.data$shutdown)
jags.data$hr <- jags.data$hr + 1
jags.data$weekday <- jags.data$weekday + 1

library(rjags)
load.module("glm")
load.module("diag")

initfun <- function() {
  list(alpha = rnorm(1),
       beta.weekday = c(NA, rnorm(6)),
       beta.holiday = rnorm(1),
       beta.hour = c(NA, rnorm(23)),
       beta.temp = rnorm(2),
       beta.humidity = rnorm(1),
       beta.windspeed = rnorm(1),
       beta.weather = c(NA, rnorm(2)),
       beta.month = c(NA, rnorm(11)),
       beta.season = c(NA, rnorm(3)))
}

mod <- jags.model("bikesharing.bug", data=jags.data, inits=initfun, n.chain=4)
update(mod, 10000)

samples <- jags.samples(mod, n.iter=10000,
                        variable.names=c("loglik.day", "casual", "casual"),
                        stat=c("value", "logdensity", "leverage"),
                        summary=c("cov","var","mean"))

## Influence by day
V <- apply(samples$value$cov$loglik.day, c(1,2), mean)
v <- diag(V)

## Leverage by hour
h.hourly <- apply(samples$leverage$mean$casual, 1, mean)
v.hourly <- apply(samples$logdensity$var$casual, 1, mean)
## Ignore periods when network is shut down
h.hourly[bikedata2011$shutdown] <- 0
v.hourly[bikedata2011$shutdown] <- 0

## Leverage by day
h <- aggregate(h.hourly, by=bikedata2011[,"dteday",drop=FALSE], FUN=sum)$x

