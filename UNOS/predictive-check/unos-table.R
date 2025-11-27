library(rjags)
load.module("glm")
load.module("diag")

set.seed(231125)
jags.seed(521132)

unos <- read.csv("../common/unos-data.csv")
unos$age <- unos$age/10

source("../common/unos-init.R")

NREPS = 500
THIN = 100

m <- jags.model("unos-reference.bug", 
                data=c(unos[,c("centre","age","n","y")]),
                n.chains=1, inits=initfun)
update(m, 20000)
yrep.samples <- jags.samples(m, "yrep", n.iter=NREPS*THIN, thin=THIN, stat="value",
                             summary="trace")$yrep

cal_diagnostics <- function(y) {

    data <- c(unos[, c("centre", "age", "n")], list("y"=y))
    
    m <- jags.model("unos-reference.bug",
                    data=data, 
                    n.chains=5,
                    inits=initfun,
                    quiet=TRUE)
    update(m, 10000, progress.bar=NULL)
    
    s <- jags.samples(m, variable.names=c("logpi", "log1mpi", "y", "y", "y"),
                      n.iter=10000, thin=10,
                      stat=c(rep("value", 2),"logdensity", "logdensity_total", "leverage"),
                      summary=c(rep("var", 4), "mean"),
                      progress.bar=NULL)

    pD <- apply(s$leverage$mean$y, 2, sum)
    
    pW.binary <- with(c(unos, s$value$var), y * logpi + (n - y) * log1mpi)
    pW.binary <- apply(pW.binary, 2, sum)
    
    pW.binomial <- apply(s$logdensity$var$y, 2, sum)
    pV <- 2 * c(s$logdensity_total$var$y)

    list(pD=pD, pW.binary=pW.binary, pW.binomial=pW.binomial, pV=pV)
}

pD.rep <- pW.rep <- pV.rep <- numeric(NREPS)
cat("\n")
for (i in 1:NREPS) {
    cat("*")
    rep.samples <- cal_diagnostics(yrep.samples[,i,1,drop=TRUE])
    pD.rep[i] <- mean(rep.samples$pD)
    pV.rep[i] <- mean(rep.samples$pV)
    pW.rep[i] <- mean(rep.samples$pW.binary)
}

