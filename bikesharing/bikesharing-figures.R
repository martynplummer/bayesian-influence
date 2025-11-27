### Run bikesharing-fit.R first to create diagnostics h, v, V

library(ggplot2)
library(dplyr)
library(scales)

dates <- as.Date("2011-01-01") + 0:364

CLLEV <- h/sum(h)
CLINF = v/sum(v)
CLOUT = CLINF/CLLEV

## Not shown in the article: there are no observations with outstandingly
## high leverage
plot(CLLEV ~ dates)

## Clean up outlier statistics with dimension reduction
Omega <- diag(1/sqrt(h)) %*% V %*% diag(1/sqrt(h)) * sum(h) / sum(diag(V))
ev <- eigen(Omega)

## Not shown in the article: a scree plot suggests informally top 7
## eigenvalues might be enough
plot(ev$values[1:50])

## Take only the top eigenvalues
m <- 7
S <- ev$vectors[,1:m]
Lambda <- diag(ev$values[1:m])
Omega7 <- S %*% Lambda %*% t(S)

CLOUT7 <- diag(Omega7)

### Prepare data for plotting

data1 <- data.frame(y = CLINF,
                    x = dates,
                    group = "clinf",
                    row.names = NULL)

data2 <- data.frame(y = CLOUT,
                    x = dates,
                    group = "clout",
                    row.names = NULL)

data3 <- data.frame(y = CLOUT7,
                    x = dates,
                    group = "clout7",
                    row.names = NULL)

plot.data <- bind_rows(data1, data2, data3) |>
    mutate(group = factor(group, levels=c("clinf", "clout", "clout7"),
                          labels=c("CLINF", "CLOUT", expression(CLOUT^{(7)}))))

### Data for outlier labelling

outlier.dates <- as.Date(c("2011-04-15", "2011-07-04", "2011-08-27",
                           "2011-09-04", "2011-11-25", "2011-12-30"))
outlier.data <- data.frame(x = outlier.dates,
                           y = subset(data3, x %in% outlier.dates, select="y",
                                      drop=TRUE),
                           hjust = c(1.05, 1.05, 1.05, 0, 1.1, 1.1),
                           vjust = c(1, 1, -0.1, 0, 1, 0),
                           label = c("Emancipation\nday weekend",
                                     "Independence day",
                                     "Hurricane\nIrene",
                                     "Labor day\nweekend",
                                     "Black\nFriday",
                                     "Holiday\nseason"),
                           group="clout7") |>
    mutate(group = factor(group, levels=c("clinf", "clout", "clout7"),
                          labels=c("CLINF", "CLOUT", expression(CLOUT^{(7)}))))

ticks <- as.Date(c(paste0("2011-", 1:12, "-1"), "2011-12-31"))
midmonth <- as.Date(paste0("2011-", 1:12, "-15"))

p <- ggplot(data=plot.data, mapping=aes(x=x, y=y, ymax=y)) +
    geom_linerange(ymin=0) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.1))) +
    facet_wrap(~ group, ncol=1, scales="free_y", labeller=label_parsed) +
    geom_text(data=outlier.data, mapping=aes(x=x, y=y, hjust=hjust, vjust=vjust,
                                             label=label)) +
    scale_x_date(breaks = midmonth,
                 minor_breaks = ticks,
                 labels = scales::label_date("%b"),                
                 guide = guide_axis(minor.ticks = TRUE),
                 expand=c(0,0)) +
    cowplot::theme_cowplot() +
    theme(axis.ticks.x = element_blank(),         
          axis.minor.ticks.x.bottom = element_line(),
          axis.minor.ticks.length.x.bottom = rel(2)) +
    xlab("Date") +
    ylab("Conformal diagnostic")

cairo_pdf("bikeshare-clout-daily.pdf", width=10, height=10)
show(p)
dev.off()


### Highlighting days that include important outliers when analysed on
### an hourly scale
clout.hourly <- v.hourly*sum(h.hourly)/(sum(v.hourly) * h.hourly)
cairo_pdf("bikeshare-clout-hourly.pdf", width=10, height=6)
par(mfrow=c(1,2), las=TRUE)
plot(clout.hourly[bikedata2011$dteday=="2011-05-02"], type="b", pch=16,
     xaxt="n", xlab="time", ylab="CLOUT", main="2011-05-02", ylim=c(0, 45))
axis(side=1, at=c(1, 7,13,19, 24), labels=c("00:00", "06:00", "12:00", "18:00", "23:00"))
plot(clout.hourly[bikedata2011$dteday=="2011-08-23"], type="b", pch=16,
     xaxt="n", xlab="time", ylab="CLOUT", main="2011-08-23", ylim=c(0, 45))
axis(side=1, at=c(1, 7,13,19, 24), labels=c("00:00", "06:00", "12:00", "18:00", "23:00"))
dev.off()
