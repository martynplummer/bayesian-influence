library(rjags)
data <- read.csv("BRIdata.csv")

load.module("glm")
load.module("diag")

m <- jags.model("bri.bug", data=data[,c("y","n")],
                inits=list(beta=0.5, w=0.4),
                n.chains=4)
update(m, 10000)
s <- coda.samples(m, n.iter=10000, variable.names=c("beta", "w", "pi.new"))
plot(s)

s2 <- jags.samples(m, n.iter=10000, variable.names="y",
                   stat=c("logdensity", "leverage"),
                   summary=c("var", "mean"))
linf <- c(apply(s2$logdensity$var$y, 1, mean))
llev <- c(apply(s2$leverage$mean$y, 1, mean))

conformal.linf <- linf/sum(linf)
conformal.llev <- llev/sum(llev)

## Check for cross-conflict
## We need to do some calculations by hand here since JAGS will not produce the pW
## penalty for binary data when given a binomial variable.

s3 <- jags.samples(m, n.iter=10000, variable.names=c("L1", "L2", "L3"),
                   stat="value", summary="var")
## Pool over chains
L1 <- apply(s3$L1, 1, mean)
L2 <- apply(s3$L2, 1, mean)
L3 <- apply(s3$L3, 1, mean)

pV <- 2 * L1
pW <- with(data, y * L2 + (n - y) * L3)

### Plot the results

library(ggplot2)
library(dplyr)
library(cowplot)
library(forcats)
theme_set(cowplot::theme_cowplot())
data |>
    mutate(r=y/n, se=sqrt(1/y + 1/(n-y)), lower=exp(log(r) - 1.96*se),
           upper= exp(log(r) + 1.96*se),
           hospital=fct_reorder(hospital, r)) |>
    ggplot(mapping=aes(x=100*r, y=hospital)) +
    geom_vline(xintercept=100*summary(s)[[1]]["pi.new","Mean"], colour="darkgrey",
               linetype="dashed") +
    geom_segment(mapping=aes(x=100*lower, xend=100*upper), colour="darkgrey") +
    geom_point(mapping=aes(size=n), shape="square") +
    scale_size_area() + # Need this to get area proportional to n
    labs(x="mortality (%)", y=NULL, title="Mortality by hospital") +
    title("Mortality") +
    theme(legend.position="none") -> p1
p1

data |>
    mutate(pV=pV, pW=pW, hospital=fct_reorder(hospital, y/n)) |>
    ggplot(mapping=aes(x=pV/pW, y=hospital)) +
    geom_point() +
    geom_segment(xend=1) +
    scale_y_discrete(labels=rep("",nrow(data))) +
    scale_x_continuous(limits=c(0, NA)) +
    labs(y=" ", x=expression(pV[i]/pW[i]), title="Cross-conflict") +
    geom_vline(xintercept=1, colour="darkgrey", linetype="dashed") -> p2
p2

cairo_pdf("bri-figure.pdf", width=8, height=4)
cowplot::plot_grid(p1, p2, labels=c("A", "B"))
dev.off()
