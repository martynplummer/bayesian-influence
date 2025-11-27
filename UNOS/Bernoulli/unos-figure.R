library(ggplot2)
library(dplyr)
library(cowplot)
library(rjags)
load.module("glm")
load.module("diag")
library(lme4)

set.seed(121125)
jags.seed(201125)

source("read-unos-data.R")
source("unos-init.R")

### Get MLEs for the fixed effects using glmer

glmer.out <- glmer(y ~ age + (1 + age | centre), family=binomial(),
                   data=unos_binary)

## Extract fixed effects and recentre the intercept
beta <- glmer.out@beta
beta[1] <- beta[1] + mean(unos_binary$age) * beta[2]

m <- jags.model("unos-reference.bug",
                data=unos_binary[,c("centre","age","y")],
                n.chains=5,
                inits=initfun)
update(m, 20000)
plot.samples <- jags.samples(m, c("mu.alpha", "mu.beta", "sigma.alpha", "sigma.beta"),
                             n.iter=100000, thin=10)

plot.data <- as.data.frame(lapply(plot.samples, c))

### Create plots

plot.list <- vector("list", 4)
theme_set(cowplot::theme_cowplot())
plot.list[[1]] <- ggplot(plot.data, mapping=aes(x=mu.alpha)) +
    geom_vline(xintercept=beta[1], col="grey") +
    geom_density() +
    labs(y="density", x=expression(mu[alpha])) +
    geom_function(fun=function(x) dnorm(x, mean=-0.9, sd=0.2), colour="red",
                  linetype="dashed") +
    scale_x_continuous(limits=c(NA, -0.25)) +
    annotate(geom="text", x=beta[1]+0.05, y=5,
             label="hat(mu)[alpha]", colour=grey(0.4), parse=TRUE)
plot.list[[2]] <- ggplot(plot.data, mapping=aes(x=mu.beta)) +
    geom_vline(xintercept=beta[2], col="grey") +
    geom_density() +
    labs(y="density", x=expression(mu[beta])) + 
    geom_function(fun=function(x) dnorm(x, mean=0.17, sd=0.05), colour="red",
                  linetype="dashed") +
    scale_x_continuous(limits=c(NA, 0.4)) +
    annotate(geom="text", x=beta[2]+0.02, y=8, label="hat(mu)[beta]",
             colour=grey(0.4), parse=TRUE)

## Not shown in the paper, we can also plot the posterior distribtions for
## the standard deviations of the random effects

plot.list[[3]] <- ggplot(plot.data, mapping=aes(x=sigma.alpha)) +
    geom_density() +
    labs(x="", y="", title=expression(sigma[alpha])) +
    geom_function(fun=function(x) 2 * dt(x, df=2), colour="red",
                  linetype="dashed")
plot.list[[4]] <- ggplot(plot.data, mapping=aes(x=sigma.beta)) +
    geom_density() +
    labs(x="", y="", title=expression(sigma[beta])) +
    geom_function(fun=function(x) 2 * dt(x, df=2), colour="red",
                  linetype="dashed")

cairo_pdf("unos-figure.pdf", width=8, height=4)
cowplot::plot_grid(plotlist=plot.list[1:2], labels=c("A", "B"))
dev.off()
