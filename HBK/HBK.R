#############################################################################
## R Script for HBK example 
#############################################################################

## Data set
library(robustbase)
## Data manipulation
library(dplyr)
library(tidyr)
## Plotting
library(ggplot2)
library(cowplot)
library(ggthemes)
## Modelling
library(rjags)
load.module("glm")
load.module("diag")
load.module("mix")
set.factory("mix::TemperedMix", type="sampler", state=FALSE)

group <- rep(c("C", "B", "A"), times=c(10, 4, 61))

## Plot the data

hbk |>
    mutate(group=group) |>
    pivot_longer(cols=c(X1, X2, X3)) |>
    ggplot(mapping=aes(x=value, y=Y, colour=group)) +
    xlab("predictor value") +
    geom_smooth(method="lm", formula=y~x, colour="grey", se=FALSE) +
    geom_point() +
    facet_wrap(~name, scales="free_x", axes="all") +
    scale_color_colorblind() +
    theme_cowplot() -> hbkplot
hbkplot

### Compare linear model with robust linear model

lm.out <- lm(Y ~ X1 + X2 + X3, data=hbk)
summary(lm.out)

lmrob.out <- lmrob(Y ~ X1 + X2 + X3, data=hbk)
summary(lmrob.out)

### Fit linear model in JAGS

set.seed(4273479)
nchain <- 5

initfun <- function() {
    list(alpha=rnorm(1), beta=rnorm(3), tau=rgamma(1,5,5))
}

m1 <- jags.model("HBKnormal.bug", data=hbk, n.chains=nchain, inits=initfun())
update(m1, 10000)
s1 <- jags.samples(m1, variable.names="Y", stat=c("logdensity", "leverage"),
                   summary=c("cov", "mean"), n.iter=100000, thin=10)

### Fit epsilon-contamination model in JAGS

initfun2 <- function() {
    list(alpha=-1, beta=rep(0,3), tau=rgamma(2, 5, 5), eps2=rbeta(1, 1, 9), mu2=10)
}

m2 <- jags.model("HBKnormmix.bug", data=hbk, n.chains=10, inits=initfun2(),
                 n.adapt=10000)
update(m2, 10000)

### Check convergence
coda.out <- coda.samples(m2, c("pi", "alpha", "beta", "mu2", "tau"),
                         n.iter=10000, thin=10)
gelman.diag(coda.out, multivariate=FALSE)

s2 <- jags.samples(m2, variable.names="Y", stat=c("logdensity", "leverage"),
                   summary=c("cov", "mean"), n.iter=100000, thin=10)


cal.maxev <- function(samples)
{
    V <- apply(samples$logdensity$cov$Y, 1:2, mean)
    h <- apply(samples$leverage$mean$Y, 1, mean)

    ev1 <- eigen(V, symmetric=TRUE)
    delta1 <- ev1$vectors[,1]
    
    hVh <- diag(1/sqrt(h)) %*% V %*% diag(1/sqrt(h))
    ev2 <- eigen(hVh, symmetric=TRUE)
    delta2 <- ev2$vectors[,1]

    ## Flip delta1 so that the largest element is positive
    if (diff(abs(range(delta1))) < 0) {
        delta1 <- -delta1
    }
    ## Flip delta2 so that inner product with delta1 is positive
    if (crossprod(delta1, delta2) < 0) {
        delta2 <- -delta2
    }

    return(list(delta1=delta1, delta2=delta2))
}

plot.maxev <- function(samples) {

    ev <- cal.maxev(samples)
    plot.data <- data.frame(x=rep(1:75, 2),
                            delta=c(ev$delta1, ev$delta2),
                            group=c(group, group),
                            diag=rep(c("influence","outlyingness"), each=75))

    ggplot(plot.data, mapping=aes(x=x, y=delta, colour=group)) +
        geom_point(pch=16) +
        geom_hline(yintercept=0, colour="grey") +
        labs(y="factor loading", x="index") +
        facet_wrap(~diag, ncol=1) +
        scale_color_colorblind() +
        theme_cowplot()
}

plot.maxev(s1)
plot.maxev(s2)

### Analysis of cross-conflict using the pV/pW ratio

grouped.conflict <- function(samples)
{
    V <- apply(samples$logdensity$cov$Y, 1:2, mean)
    
    conflict <- function(g) {
        inset <- group==g
        2*sum(V[inset, inset])/sum(diag(V)[inset])
    }

    sapply(LETTERS[1:3], conflict)
}

grouped.conflict(s1)
grouped.conflict(s2)


cairo_pdf("hbk-data.pdf", width=12, height=4)
hbkplot
dev.off()

cairo_pdf("hbk-influence.pdf", width=8, height=4)
plot.maxev(s2)
dev.off()
