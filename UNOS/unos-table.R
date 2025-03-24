library(rjags)
load.module("glm")
load.module("diag")

unos <- read.csv("unos-data.csv")
unos$age <- unos$age/10


cal_diagnostics <- function(model.file, ...) {

    ## Optional arguments (...) are fixed hyper-parameters passed to
    ## the model as data
    data <- c(unos[, c("centre", "age", "n", "y")], ...)
    
    m <- jags.model(model.file,
                    data=data, 
                    n.chains=5,
                    inits=list(mu.alpha=-1, mu.beta=0.1, tau.alpha=10, tau.beta=10))
    update(m, 50000)
    
    s <- jags.samples(m, variable.names=c("mu.alpha", "sigma.alpha",
                                          "logpi", "log1mpi", "y", "y", "y"),
                      n.iter=100000, thin=20,
                      stat=c(rep("value", 4),"logdensity", "logdensity_total", "leverage"),
                      summary=rep(c("trace","var", "mean"), times=c(2, 4, 1)))

    pD <- apply(s$leverage$mean$y, 2, sum)
    
    pW.binary <- with(c(unos, s$value$var), y * logpi + (n - y) * log1mpi)
    pW.binary <- apply(pW.binary, 2, sum)
    
    pW.binomial <- apply(s$logdensity$var$y, 2, sum)
    pV <- 2 * c(s$logdensity_total$var$y)

    list(pD=pD, pW.binary=pW.binary, pW.binomial=pW.binomial, pV=pV,
         mu.alpha = s$value$trace$mu.alpha,
         sigma.alpha = s$value$trace$sigma.alpha)
}

## Reference posterior
reference.samples <- list(cal_diagnostics("unos-reference.bug"))

## Studying the effect of shifting the prior mean away from the
## reference posterior
aseq <- c(-0.9, -0.7, -0.5, -0.3, -0.1)
shift.samples <- vector("list", length(aseq))
for (i in seq_along(shift.samples)) {
    shift.samples[[i]] <- cal_diagnostics("unos-informative.bug", a=aseq[i], b=0.2)
}

## Studying the effect of increasingly concentrated prior on mu.alpha
bseq <- c(0.2, 0.1, 0.05, 0.02, 0.01)
concentration.samples <- vector("list", length(bseq))
for (i in seq_along(concentration.samples)) {
    concentration.samples[[i]] <- cal_diagnostics("unos-informative.bug", a=-0.9, b=bseq[i])
}

make_table <- function(samples) {
    pD <- sapply(samples, \(x) mean(x$pD))
    pW <- sapply(samples, \(x) mean(x$pW.binary))
    pV <- sapply(samples, \(x) mean(x$pV))
    
    data.frame("pD" = round(pD, 1),
               "pW" = round(pW, 1),
               "pV" = round(pV, 1),
               "pV.PW" = round(pV/pW, 1))
}

tab <- rbind(data.frame(prior.mean=0, prior.sd=NA,
                        make_table(reference.samples)),
             data.frame(prior.mean=aseq, prior.sd=0.2,
                        make_table(shift.samples)),
             data.frame(prior.mean=-0.9, prior.sd=bseq,
                        make_table(concentration.samples)))

knitr::kable(tab, format="latex")

