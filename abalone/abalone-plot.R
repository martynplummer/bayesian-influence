source("abalone-fit.R")
id <- abalone$id

library(ggplot2)

### Identify which observations have highest leverage, influence, or outlyingness
high.cllev = which(rank(-cllev) %in% 1:2)
high.clinf = which(rank(-clinf) %in% 1:2)
high.clout = which(rank(-clout) %in% 1:2)

plot.data <- data.frame(diag=rep(c("leverage","influence","influence/leverage"),
                                 times=rep(nrow(abalone), 3)),
                        id = rep(id, times=3),
                        y=c(cllev, clinf, clout))

text.data <- data.frame(diag=rep(c("leverage","influence","influence/leverage"),
                                 times=rep(2,3)),
                        id = id[c(high.cllev, high.clinf, high.clout)],
                        y=c(cllev[high.cllev], clinf[high.clinf], clout[high.clout]))

p <- ggplot(data=plot.data, mapping=aes(x=id, y=y, ymax=y)) +
    geom_linerange(ymin=0) +
    labs(x="observation index", y="conformal diagnostic") +
    geom_text(data=text.data, mapping=aes(label=id), hjust=1.1, vjust=0.5) +
    facet_wrap(~ forcats::fct_relevel(diag, "leverage","influence","influence/leverage"),
               dir="v", scales="free_y") +
    cowplot::theme_cowplot()

cairo_pdf("abalone-influence.pdf", width=8, height=8)
p
dev.off()

### Create table with data on anomalous observations
anomalous <- unique(c(high.clinf, high.cllev, high.clout))
sink("hightab.tex")
knitr::kable(abalone[anomalous,], format="latex")
sink()
