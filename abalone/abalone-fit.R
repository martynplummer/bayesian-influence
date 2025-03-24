source("read-data.R")

glm.out <- glm(shucked.weight ~ sex + log(length) + log(diameter) +
                   log(height) + log(whole.weight),
               family=Gamma(link="log"), data=abalone)
summary(glm.out)

library(rjags)
load.module("diag")

jags.data <- vector("list")
jags.data$sex <- ifelse(abalone$sex=="F", 1, 0)
jags.data$llength <- c(scale(log(abalone$length), T, F))
jags.data$ldiameter <- c(scale(log(abalone$diameter), T, F))
jags.data$lheight <- c(scale(log(abalone$height), T, F))
jags.data$lwhole.weight <- c(scale(log(abalone$whole.weight), T, F))
jags.data$shucked.weight <- abalone$shucked.weight
jags.data$N <- nrow(abalone)

mod <- jags.model("abalone.bug", data=jags.data,
                  inits=list(alpha=0, beta=rep(0,5), shape=1),
                  n.chains=4)
update(mod, 2000)

## Diagnostics

diag.samples <- jags.samples(mod, "shucked.weight", n.iter=1000,
                             stat=c("logdensity", "leverage"), summary=c("var","mean"))

## Calculate conformal local influence clinf
linf <- diag.samples$logdensity$var$shucked.weight
linf <- apply(linf, 1, mean)
clinf <- linf/sum(linf)

## Calculate conformal local leverage cllev
llev <- diag.samples$leverage$mean$shucked.weight
llev <- apply(llev, 1, mean)
cllev <- llev/sum(llev)

## Calculate conformal outlyingness clout
clout <- clinf/cllev
